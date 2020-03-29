# world data 
library(plotly)
library(lubridate)


# to do: 
# add graphs 
# confirmed cases per million population
# deaths per million population
# temperature scatterplots vs deaths, confimed cases etx

gg_panel_temp_world <- 
  ggplot(all_data %>% filter(Country != "US"), 
       aes(date, temp, colour = Country, group = Country)) +
  geom_line() +
  theme(legend.position="none") +
  controls +
  ggtitle("Median daily temperature in world countries")

ggplotly(gg_panel_temp_world)

gg_panel_temp_usa <- 
  ggplot(all_data %>% filter(Country == "US"), 
       aes(date, temp, colour = State, group = State)) +
  geom_line() +
  theme(legend.position = "none") +
  controls +
  ggtitle("Median daily temperature in usa states")

ggplotly(gg_panel_temp_usa)

temp_summary <- 
  all_data %>%
  group_by(Country, code, State) %>%
  summarize(temp_sd = sd(temp, na.rm = T), 
            temp_mean = mean(temp, na.rm = T),
            temp_median = median(temp, na.rm =))

gg_sd_temp_usa <- 
  ggplot(temp_summary %>% filter(Country == "US"), 
       aes(reorder(State, temp_sd), temp_sd)) +
  geom_bar(stat = "identity") +
  controls +
  coord_flip() +
  ggtitle(paste("SD of temperature from ", min(all_data$date), "till",
                max(all_data$date)))

gg_sd_temp_world <- 
  ggplot(temp_summary %>% filter(Country != "US"), 
         aes(reorder(Country, temp_sd), temp_sd)) +
  geom_bar(stat = "identity") +
  controls +
  coord_flip() +
  ggtitle(paste("SD of temperature from ", min(all_data$date), "till",
                max(all_data$date)))



gg_covid_deaths_world <- 
  ggplot(all_data %>% filter(Country != "US"), 
         aes(date, deaths, colour = Country, group = Country)) +
  geom_line() +
  theme(legend.position="none") +
  controls +
  ggtitle("COVID-19 deaths")



gg_covid_deaths_usa <- 
  ggplot(all_data %>% filter(Country == "US"), 
         aes(date, deaths, colour = State, group = State)) +
  geom_line() +
  theme(legend.position="none") +
  controls +
  ggtitle("COVID-19 deaths")


rmarkdown::render(
  input =
    "weatherAnalysis_data_exploration.rmd",
  output_dir = "reports",
  output_file = paste0("Data overview",
                       "_",
                       date(now()),
                       ".html"
  )
)


