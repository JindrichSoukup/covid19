library(readr)
library(tidyr)


confirmed <- read_csv("..\\COVID-19\\csse_covid_19_data\\csse_covid_19_time_series\\time_series_19-covid-Confirmed.csv")
deaths <- read_csv("..\\COVID-19\\csse_covid_19_data\\csse_covid_19_time_series\\time_series_19-covid-Deaths.csv")
recovered <- read_csv("..\\COVID-19\\csse_covid_19_data\\csse_covid_19_time_series\\time_series_19-covid-Recovered.csv")

confirmed_usa <- 
  confirmed %>%
  dplyr::rename(Country = `Country/Region`, State = `Province/State`) %>%
  filter(Country == "US") %>%
  dplyr::select(-Lat, -Long)

usa_states <- 
  confirmed_usa %>%
  # lets not take cruise ship
  filter(State != "Diamond Princess") %>%
  # lets not take district of columnbia since its teritory with 700k people
  filter(State != "District of Columbia") %>%
  pull(State) %>%
  head(50) %>% 
  sort()

confirmed_final <- 
  confirmed_usa %>%
  filter(State %in% usa_states) %>%
  gather(date, confirmed, -State, -Country)

deaths_final <- 
  deaths %>%
  dplyr::rename(Country = `Country/Region`, State = `Province/State`) %>%
  filter(Country == "US" & State %in% usa_states) %>%
  dplyr::select(-Lat, - Long) %>%
  gather(date, deaths, -State, -Country)

recovered_final <- 
  recovered %>%
  dplyr::rename(Country = `Country/Region`, State = `Province/State`) %>%
  filter(Country == "US" & State %in% usa_states) %>%
  dplyr::select(-Lat, - Long) %>%
  gather(date, recovered, -State, -Country)

# now bind all

usa_data <- 
  confirmed_final %>%
  left_join(deaths_final) %>%
  left_join(recovered_final)
  
