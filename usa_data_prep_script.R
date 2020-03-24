library(readr)
library(tidyr)
library(ggplot2)


confirmed <- read_csv("..\\COVID-19\\csse_covid_19_data\\csse_covid_19_time_series\\time_series_19-covid-Confirmed.csv")
deaths <- read_csv("..\\COVID-19\\csse_covid_19_data\\csse_covid_19_time_series\\time_series_19-covid-Deaths.csv")
recovered <- read_csv("..\\COVID-19\\csse_covid_19_data\\csse_covid_19_time_series\\time_series_19-covid-Recovered.csv")

schools <- read_csv("schools_closing_usa.csv")

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

# add schools data
schools_outage <- 
  schools %>% 
  dplyr::select(State, `State Closure Start Date`) %>%
  mutate(schoolOutageDate = as.Date(`State Closure Start Date`, "%m/%d/%y")) %>%
  dplyr::select(-`State Closure Start Date`)

usa_all <- 
  usa_data %>%
  left_join(schools_outage) %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  filter(!is.na(schoolOutageDate)) %>%
  arrange(State, date) %>%
  group_by(State) %>%
  mutate(`School outage` = ifelse(date < schoolOutageDate, 0, 1)) %>%
  mutate(first_case_day = confirmed == 1)

first_case <-   
  usa_all %>%
  group_by(State) %>%
  filter(confirmed != 0) %>%
  arrange(date) %>%
  slice(1) %>%
  mutate(first_case_day = date, 
         number_first_reported = confirmed) %>%
  dplyr::select(State, first_case_day, number_first_reported)
  
ggplot(first_case, aes(number_first_reported)) +
  geom_histogram() 

usa_data_ready <- 
  usa_all %>%
  left_join(first_case) %>%
  mutate(first_case_schools_closures = difftime(schoolOutageDate, first_case_day, 
                                                units = c("days"))) 


  
