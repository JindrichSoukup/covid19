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
    temp_part <- read.csv(paste0("temp2020/", fl))
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