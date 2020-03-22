# Dependencies =================================================================
library(jsonlite)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)



# Parameters ===================================================================
THRESHOLD_PEOPLE <- 100000


# Download data ================================================================
json <- fromJSON("https://pomber.github.io/covid19/timeseries.json")
#json <- fromJSON("timeseries.json")
df <- bind_rows(json, .id = "column_label")
population <- read_csv("world_bank_population_data.csv")


# clean up and merge datasets
# adding character to collumn names since now these are numbers
colnames(population) <- paste0("a", colnames(population))
pop <-
  population %>%
  dplyr::rename(country = "aCountry Name") %>%
  select(country, a2011) %>%
  dplyr::rename(population = a2011) %>%
  mutate(country = tolower(country)) %>%
  mutate(country = gsub(" ", "_", country))


dat <- 
  df %>%
  dplyr::rename(country = `column_label`) %>%
  mutate(country = tolower(country))

df <- 
  df %>%
  dplyr::rename(country = column_label) %>%
  mutate(country = tolower(gsub(" ", "_", country))) %>%
  mutate(country = ifelse(country == "korea,_south", "korea", country))

# create country population map
country_pop <- 
  data.frame(country = unique(df$country)) %>%
  left_join(pop) %>%
  arrange(country)

# how many countries are we missing in population
sum(is.na(country_pop$population))
missing_pop <- 
  country_pop %>%
  filter(is.na(population)) 
unique(missing_pop$country)

# so this is country population map to be used to join to cases data

