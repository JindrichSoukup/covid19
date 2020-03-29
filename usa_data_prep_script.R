john_hopkins_data <- F
nytimes_data <- T


# taken from https://www.edweek.org/ew/section/multimedia/map-coronavirus-and-school-closures.html 
schools <- read_csv("schools_closing_usa.csv")
# taken from https://worldpopulationreview.com/states/state-abbreviations/
usa_states_codes <- read_csv("usa_states_codes.csv")
usa_states_population <- read_excel("usa_states_population.xlsx", sheet = 1, 
                                    na = "NA")

if (john_hopkins_data) {
  # to have this you need to clone next to our repository folder
  # https://github.com/CSSEGISandData/COVID-19
  # they are changing which data they show so update in code 
  # might be needed
  
  confirmed <- read_csv("..\\john_hopkins_repo\\COVID-19\\archived_data\\archived_time_series\\time_series_19-covid-Confirmed_archived_0325.csv")
  deaths <- read_csv("..\\john_hopkins_repo\\COVID-19\\archived_data\\archived_time_series\\time_series_19-covid-Deaths_archived_0325.csv")
  recovered <- read_csv("..\\john_hopkins_repo\\COVID-19\\archived_data\\archived_time_series\\time_series_19-covid-Recovered_archived_0325.csv")
  
 
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
}

head(usa_data)

if(nytimes_data) {
  nytimes_dat <- read_csv("..\\nytimes_repo\\covid-19-data\\us-states.csv")
  usa_data <-  
    nytimes_dat %>%
    dplyr::rename(State = state, confirmed = cases) %>%
    mutate(recovered = NA, Country = "US") %>%
    select(State, Country, date, confirmed, deaths, recovered)
}

# do we have all states here? these are missing states
usa_states_codes$State[!usa_states_codes$State  %in% (usa_data$State %>% unique())]
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
  mutate(`School outage` = ifelse(date < schoolOutageDate, 0, 1)) 

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
  geom_histogram() + controls + 
  ggtitle("USA number of cases reported on first day reported")



usa_weather_data <- getUSAWeatherData(path_to_data = "temp2020", 
                              path_to_usa_files_names = "usa_files_names.csv")


usa_pop <- usa_states_population[9:59, c(1, 13)] 
colnames(usa_pop) <- c("State", "population")

usa_data <- 
  usa_all %>%
  left_join(first_case, by = c("State")) %>%
  mutate(first_case_schools_closures = difftime(schoolOutageDate, first_case_day, 
                                                units = c("days"))) %>%
  left_join(usa_states_codes, by = "State") %>%
  left_join(usa_weather_data %>% dplyr::rename(code = state),
            by = c("date", "code")) %>%
  left_join(usa_pop, by = c("State")) %>%
  dplyr::select(Country, State, date, confirmed, deaths, recovered, 
                `School outage`, schoolOutageDate, code, temp, 
                number_first_reported, first_case_schools_closures, population)


