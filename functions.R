library(ggplot2)
controls <-
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.title.y = element_text(face = "bold", size = 16),
        axis.title.x = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        legend.direction = "vertical")



getUSACases <- function(john_hopkins_data = F, 
                        nytimes_data = T, 
                        hopkins_data_confirmed = "..\\john_hopkins_repo\\COVID-19\\archived_data\\archived_time_series\\time_series_19-covid-Confirmed_archived_0325.csv", 
                        hopkins_data_deaths = "..\\john_hopkins_repo\\COVID-19\\archived_data\\archived_time_series\\time_series_19-covid-Deaths_archived_0325.csv", 
                        hopkins_data_recovered = "..\\john_hopkins_repo\\COVID-19\\archived_data\\archived_time_series\\time_series_19-covid-Recovered_archived_0325.csv",
                        path_nytimes = "..\\nytimes_repo\\covid-19-data\\us-states.csv") {
  #john_hopkins_data <- F # clone https://github.com/CSSEGISandData/COVID-19 to
  # john_hopkins_repo to be next to our repo
  #nytimes_data <- T # clone https://github.com/nytimes/covid-19-data.git to 
  # nytimes_repo to be next to our repo 
  
  
  if (john_hopkins_data) {
    
    confirmed <- read_csv(hopkins_data_confirmed )
    deaths <- read_csv( hopkins_data_deaths)
    recovered <- read_csv(hopkins_data_recovered )
    
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
  
  
  if(nytimes_data) {
    nytimes_dat <- read_csv(path_nytimes)
    
    frame_data <- 
      nytimes_dat %>%
      dplyr::select(date, state, cases) %>%
      spread(state, cases) %>%
      gather(state, cases, -date) %>%
      dplyr::select(state, date) %>%
      distinct()
    
    usa_data <- 
      frame_data %>%
      left_join(nytimes_dat, by = c("state", "date")) %>%
      mutate(cases =  ifelse(is.na(cases), 0, cases), 
             deaths = ifelse(is.na(deaths), 0, deaths)) %>%
      dplyr::rename(State = state, confirmed = cases) %>%
      mutate(recovered = NA, Country = "US") %>%
      dplyr::select(State, Country, date, confirmed, deaths, recovered)
  }
  
  return(usa_data)
  
}
  
  
getUSAData <- function(john_hopkins_data = F, 
                           nytimes_data = T, 
                           path_nytimes = "..\\nytimes_repo\\covid-19-data\\us-states.csv", 
                           path_hopkins_confirmed = "..\\john_hopkins_repo\\COVID-19\\archived_data\\archived_time_series\\time_series_19-covid-Confirmed_archived_0325.csv", 
                           path_hopkins_deaths = "..\\john_hopkins_repo\\COVID-19\\archived_data\\archived_time_series\\time_series_19-covid-Confirmed_archived_0325.csv", 
                           path_hopkins_deathspath_hopkins_deaths = "..\\john_hopkins_repo\\COVID-19\\archived_data\\archived_time_series\\time_series_19-covid-Recovered_archived_0325.csv",
                           path_schools_data = "schools_closing_usa.csv", 
                           path_states_codes = "usa_states_codes.csv", 
                           path_population = "usa_states_population.xlsx", 
                           path_to_weather_data = "temp2020//2020//", 
                           path_to_weather_usa_states_names = "usa_files_names.csv"){
  
  # taken from https://www.edweek.org/ew/section/multimedia/map-coronavirus-and-school-closures.html 
  schools <- read_csv(path_schools_data)
  # taken from https://worldpopulationreview.com/states/state-abbreviations/
  usa_states_codes <- read_csv(path_states_codes)
  usa_states_population <- read_excel(path_population, sheet = 1, 
                                      na = "NA")
  usa_data <- getUSACases(john_hopkins_data = F, 
                          hopkins_data_confirmed = path_hopkins_confirmed,
                          hopkins_data_deaths = path_hopkins_deaths, 
                          hopkins_data_recovered = path_hopkins_deaths,
                          nytimes_data = T, 
                          path_nytimes =  path_nytimes)
  
  
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
  
  
  
  usa_weather_data <- getUSAWeatherData(path_to_data = path_to_weather_data, 
                     path_to_usa_files_names = path_to_weather_usa_states_names)
  
  
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
  
  return(usa_data)
} 


getUSAWeatherData <- function(path_to_data,
                              path_to_usa_files_names = "usa_files_names.csv") {
  t1 <- Sys.time()
  # test values
  #path_to_data <- "temp2020"
  
  if(is.na(path_to_usa_files_names)){
    fls <- dir(path_to_data)
  } else(
    fls <- read_csv(path_to_usa_files_names) %>% pull(file_name)
  )
  
  all_data <- NULL
  for (fl in fls) {
    print(fl)
    temp_part <- read.csv(paste0(path_to_data, fl))
    temp_to_keep <- temp_part %>% dplyr::select(DATE, NAME, TEMP) 
    station_name <- str_split(temp_part$NAME[1], pattern = ",")[[1]][2]
    if(sjmisc::str_contains(station_name,  "US")) {
      all_data <- rbind(all_data, temp_to_keep %>% mutate(file_name = fl))
    } else {next}
  }
  
  if(is.na(path_to_usa_files_names)) {
    usa_files_names <-
      all_data %>%
      dplyr::select(file_name) %>%
      unique()
    write.csv(usa_files_names, "usa_files_names.csv", row.names = F)
  }
  
  
  all_data_cleaned <- 
    all_data %>% 
    separate(NAME, c("city", "state_country"), sep = ",") %>%
    mutate(state_country = substr(state_country, 2, nchar(state_country))) %>%
    separate(state_country, c("state", "country"), sep = " ")
  
  missing_state_code <- 
    all_data_cleaned %>%
    filter(is.na(country)) %>%
    pull(city) %>% 
    unique()
  
  print("These are locations with missing state attribution")
  print(missing_state_code)   

  
  weather_usa <- 
    all_data_cleaned %>%
    filter(!is.na(state)) %>%
    group_by(state, DATE) %>%
    summarize(temp = median(TEMP)) %>%
    mutate(country = "US", date = as.Date(DATE)) %>%
    dplyr::select(country, state, date, temp)
   
  t2 <- Sys.time()
  message(paste("It took that much time", t2 - t1, "in minutes"))
  return(weather_usa)
}
