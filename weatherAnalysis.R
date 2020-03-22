library(jsonlite)
library(dplyr)
library(readr)
library(MASS)
library(httr)

#setwd("C:/Users/Jindra/Documents/R/COVID19")

GET('https://pomber.github.io/covid19/timeseries.json', write_disk(tf <- tempfile(fileext = ".json")))
a <- fromJSON(tf)

#a <- fromJSON("timeseries.json")

df <- bind_rows(a, .id = "Country")

df.filtered <- df %>%
  filter(confirmed > 100) %>%
  filter(confirmed < 400)

df2 <- df.filtered %>%
  group_by(Country) %>%
  filter(n()>2) %>%
  summarise(increase = (last(log(confirmed)) - first(log(confirmed))) / (n()-1) )

temp <- read_csv("temperatures.csv", col_names = F)
names(temp) <- c("Country", "temp")

df3 <- left_join(df2, temp)

# Without NA and Diamond Princess cruise ship
df4 <- df3[-which(df3$Country == "Cruise Ship"),]
df4 <- df4[!is.na(df4$temp), ]
plot(df4$temp, df4$increase)

model <- lm(increase ~ temp, 
            data = df4)
summary(model)

cor(df4$temp, df4$increase)
cor.test(df4$temp, df4$increase)

rmodel <- rlm(increase ~ temp, 
              data=df4)
summary(rmodel)

cov.rob(df4[, 2:3], nsamp = "exact")

# Without Iran and Pakistan
df5 <- df4
df5 <- df5[-which(df5$Country == "Iran"), ]
df5 <- df5[-which(df5$Country == "Pakistan"), ]
plot(df5$temp, df5$increase)

model <- lm(increase ~ temp, 
            data = df5)
summary(model)

cor(df5$temp, df5$increase)
cor.test(df5$temp, df5$increase)

rmodel <- rlm(increase ~ temp, 
              data=df5)
summary(rmodel)

cov.rob(df5[, 2:3], nsamp = "exact")

## GDP

GDP <- read_csv("GDP_PPP_per_capita.csv")
names(GDP) <- c("Country", "GDP_PPP", "GDP_nominal")

GDP$GDP_PPP[GDP$GDP_PPP == "N.A."] <- NA
GDP$GDP_PPP <- substr(GDP$GDP_PPP, 2, nchar(GDP$GDP_PPP))
GDP$GDP_PPP <- as.numeric(gsub(",", "", GDP$GDP_PPP))

GDP$GDP_nominal[GDP$GDP_nominal == "N.A."] <- NA
GDP$GDP_nominal <- substr(GDP$GDP_nominal, 2, nchar(GDP$GDP_nominal))
GDP$GDP_nominal <- as.numeric(gsub(",", "", GDP$GDP_nominal))

df4p <- left_join(df4, GDP)
                  
model <- lm(increase ~ GDP_nominal, 
            data=df4p)
summary(model)

df5p <- left_join(df5, GDP)
model <- lm(increase ~ GDP_nominal, 
            data=df5p)
summary(model)

## Population

pop <- read_csv("population.csv")
pop <- pop[, c(1:2, 5)]
names(pop) <- c("Country", "population", "density")


## School outage

sch <- read_csv("school_outage2.csv")
sch$Country[sch$Country=="Czech Republic"] <- "Czechia"
sch$Country[sch$Country=="Salvador"] <- "El Salvador"
sch$Country[sch$Country=="UK"] <- "United Kingdom"
sch$Country[sch$Country=="JAR"] <- "South Africa"
sch$Country[sch$Country=="Afganistan"] <- "Afghanistan"
sch$Country[sch$Country=="Kyrgystan"] <- "Kyrgyzstan"
sch$Country[sch$Country=="Quatar"] <- "Qatar"
sch$Country[sch$Country=="South Korea"] <- "Korea, South"
sch$Country[sch$Country=="Bosna and Herzegovina"] <- "Bosnia and Herzegovina"
sch$Country[sch$Country=="Guinea-Bissai"] <- "Equatorial Guinea"
setdiff(sch$Country, df$Country)
dfs <- left_join(df, sch)

dfs <- dfs[!is.na(dfs$`School outage`), ] # only countries where schools are fully closed
dfs <- dfs[dfs$`School outage`==1, ] # only countries where schools are fully closed
dfs$schoolOutageDate <- as.Date(paste(as.character(dfs$Day), as.character(dfs$Month), as.character(2020), sep = "/"), "%d/%m/%Y")
2420
dfs <- dfs[, -c(6:8)]

dfs$date <- as.Date(dfs$date, "%Y-%m-%d")
