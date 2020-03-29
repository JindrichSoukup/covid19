# dependencies =================================================================
library(jsonlite)
library(dplyr)
library(readr)
library(MASS)
library(httr)
library(stringr)
library(countrycode)
library(tidyr)
library(sjmics)
library(ggplot2)
library(readxl)

source("functions.R")

# get USA data with weather
source("usa_data_prep_script.R")

## Get confirmed cases ---

GET('https://pomber.github.io/covid19/timeseries.json', write_disk(tf <- tempfile(fileext = ".json")))
a <- fromJSON(tf)

#a <- fromJSON("timeseries.json") # if no internet connection available

df <- bind_rows(a, .id = "Country")

df$date <- as.Date(df$date, "%Y-%m-%d")
df$yday <- (as.POSIXlt(df$date))$yday
max(df$date)

## School outage ----

sch <- read_csv("school_outage2.csv")

# Fix country names
sch$Country[sch$Country == "Czech Republic"] <- "Czechia"
sch$Country[sch$Country == "Salvador"] <- "El Salvador"
sch$Country[sch$Country == "UK"] <- "United Kingdom"
sch$Country[sch$Country == "JAR"] <- "South Africa"
sch$Country[sch$Country == "Afganistan"] <- "Afghanistan"
sch$Country[sch$Country == "Kyrgystan"] <- "Kyrgyzstan"
sch$Country[sch$Country == "Quatar"] <- "Qatar"
sch$Country[sch$Country == "South Korea"] <- "Korea, South"
sch$Country[sch$Country == "Bosna and Herzegovina"] <- "Bosnia and Herzegovina"
sch$Country[sch$Country == "Guinea-Bissai"] <- "Equatorial Guinea"

# Check
setdiff(sch$Country, df$Country)

# Merge
dfs <- left_join(df, sch)

# Filter
dfs <- dfs[!is.na(dfs$`School outage`), ] # only countries where schools are fully closed
dfs <- dfs[dfs$`School outage` == 1, ] # only countries where schools are fully closed

# Clean
dfs$schoolOutageDate <- as.Date(paste(as.character(dfs$Day), as.character(dfs$Month), as.character(2020), sep = "/"), "%d/%m/%Y")
dfs <- dfs[, -c(6:8)]

## Country codes

# get list of files for each country
fls <- dir("temp2020_2")
codetable <- data.frame("Country" = countrycode::codelist$country.name.en, "code" = countrycode::codelist$fips)
dfsc <- left_join(dfs, codetable)
dfsc$code[dfsc$Country == "Korea, South"] <- "KS"

## Temperature mining

calculated <- TRUE

if (calculated) {
  x <- readRDS("temps.rds")
}else { # takes about 10 minutes
  fls <- dir("temp2020") # extracted content of https://www.ncei.noaa.gov/data/global-summary-of-the-day/archive/2020.tar.gz
  # Reduce size of the files
  for (fl in fls) {
    temp_part <- read.csv(paste0("temp2020/", fl))
    station_name <- temp_part$NAME[1]
    station_name <- gsub("\\"," ", station_name, fixed = T)
    write.csv(temp_part$TEMP, paste0("temp2020_2/",station_name, ".csv"), row.names = F)
  }
  
  
  
  temp.stations <- list()
  for (code in unique(dfsc$code)) {
    temp.stations[[code]] <- names(which(sapply(fls, function(x) sjmisc::str_contains(x, paste0(code,".csv")))))
  }
  
  # create one file for country with with all the stations as columns
  ind <- 1
  temps.all <- list()
  for (country in temp.stations) {
    temps <- list()
    for (station in country) {
      tmps <- read.csv(paste0("temp2020_2/", station))
      if (dim(tmps)[1] == 79) { # FIXME works only for 2020.tar.gz file downloaded today, TODO make it more robust
        temps[[station]] <- tmps  
      }
      #bind_rows(temps, .id = "station_name")
      
      temps.df <- data.frame(temps)
      #write.csv(temps.df, file = paste0("temp2020_3/", names(temp.stations)[ind]), row.names = F)
    }
    temps.all[[names(temp.stations)[ind]]] <- data.frame(cbind(names(temp.stations)[ind], t(temps.df)))
    ind <- ind + 1
  }
  
  x <- bind_rows(temps.all)
  x <- x[,-81]
  x2 <- x[,-1] %>% mutate_all(as.numeric)
  x[, 2:80] <- x2 
  
  x <- x %>% 
    group_by(X1) %>% 
    summarise_all(median) %>%
    gather("date", "temp", -1)
  
  x$date <- as.Date(as.numeric(str_remove(x$date, "X")) - 1, origin = "2020-01-01")
  names(x)[1] <- "code"
  
  rm(x2)
  
  saveRDS(x, "temps.rds")
}

dfsct <- left_join(dfsc, x)
usa_data_to_join <- 
  usa_data %>% 
  ungroup() %>%
  dplyr::select(-number_first_reported, -first_case_schools_closures, -population)
all_data <- bind_rows(usa_data_to_join, dfsct)

## Filter to relevant days for each country ---

daysAfterSchoolOutageLimit <- 2
confirmedLimit <- 10 # minimal number of cases for which we start 
minPopulationLimit <- 1e6

pop <- read_csv("population.csv")
pop <- pop[, c(1:2, 5)]
names(pop) <- c("Country", "population", "density")

dfsctf <- dfsct %>% 
  filter(date - schoolOutageDate < daysAfterSchoolOutageLimit) %>% # max daysAfterSchoolOutageLimit days after school outage day
  filter(confirmed > confirmedLimit) %>% # at least confirmedLimit cases 
  filter(Country %in% pop$Country[pop$population > minPopulationLimit]) # remove microcountries

dfsctff <- dfsctf[dfsctf$Country %in% names(table(dfsctf$Country)[table(dfsctf$Country) > 2]),] # at least 3 valid days

length(unique(dfsctff$Country)) # Final number of countries

## Analysis - assumption checking ---

# potential variables to be added:
# - number of days waited from first infection to school outage
# number of first day confirmed cases - if they are very many
# on the first day of reporting it might mean that testing was not
# available, missing to test in the begining might make faster growth then 
# later


## Analysis - regression ---

df2 <- dfsctff %>%
  group_by(Country) %>%
  summarise(increase = (last(log(confirmed)) - first(log(confirmed))) / (n()-1),
            temp = mean(temp, na.rm = T)) # TODO fit all points by exp instead of taking first and last

model <- lm(increase ~ temp, data = df2)
summary(model) # here the temperature seem to have negligible role 

# TODO try different parameters, see "Filter to relevant days for each country" section
# TODO maybe school outage isn't a best indicator ... :
dfsctff %>% group_by(Country) %>% summarise(n()) %>% View() # very different number of days
# TODO for big countries increase confirmedLimit 
# TODO remove China and Japan