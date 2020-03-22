# Description - this script prepares map between population data countries
# for world and usa states and those names for cases data
# Dependencies =================================================================
library(jsonlite)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)

# Download data ================================================================
json <- fromJSON("https://pomber.github.io/covid19/timeseries.json")
#json <- fromJSON("timeseries.json")
df <- bind_rows(json, .id = "column_label")
population <- read_csv("world_bank_population_data.csv")
usa_states_population <- read_excel("usa_states_population.xlsx", sheet = 1, 
                                    na = "NA")

# world data ===================================================================
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
world_pop <- 
  country_pop %>%
  mutate(state = country) %>%
  select(state, population, country)

head(world_pop)
# usa data =====================================================================
# usa data taken 2019 estimate
usa_pop <- usa_states_population[9:59, c(1, 13)] 
colnames(usa_pop) <- c("state", "population")
usa_pop$country <- "us"
head(usa_pop)
# merge all population data ====================================================
# this will be used for filtering countries more than million inhabitants
all_pop <- bind_rows(world_pop, usa_pop) %>%
           rename(State = state, Country = country)



