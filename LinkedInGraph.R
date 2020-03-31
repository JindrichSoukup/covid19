library(jsonlite)
library(ggplot2)
library(httr)
library(lubridate)
library(dplyr)

# Old deprecated API
#GET('https://api.apify.com/newCases/key-value-stores/K373S4uCFR9W1K8ei/records/LATEST?disableRedirect=true&fbclid=IwAR04Mh9ckNFHaKcy7cRSbwh6Z0snKln1oqEJGz_1H42g_M9AGG9M8P75J1M', write_disk(tf <- tempfile(fileext = ".json")))

#a <- fromJSON(tf)

# Parse JSON, take relevant items
#tt <- a$numberOfTestedGraph
#pos <- a$totalPositiveTests
#d <- cbind(tt$value, pos$value)

#beginningDay <- 35 # ~beginning of March
#endDay <- dim(d)[1] # changes every day
#df <- data.frame(dailyTested = diff(d[beginningDay:endDay, 1]),
#                 newCases = diff(d[beginningDay:endDay, 2]),
#                 totalConfirmed = d[(beginningDay+1):endDay, 2],
#                 Date = ymd(asDate(tt$date[(beginningDay+1):endDay])))

# New API

GET("https://onemocneni-aktualne.mzcr.cz/api/v1/covid-19/nakaza.json", write_disk(tf <- tempfile(fileext = ".json")))
infected <- fromJSON(tf)

#GET("https://onemocneni-aktualne.mzcr.cz/api/v1/covid-19/testy.json", write_disk(tf <- tempfile(fileext = ".json")))
#tested <- fromJSON(tf)

#dailyTested <- tested$data["testy-den"]

beginningDay <- 35 # ~beginning of March
endDay <- dim(infected)[1] # changes every day
df2 <- data.frame(newCases = infected$pocetDen[beginningDay:endDay],
                 totalConfirmed = infected$pocetCelkem[beginningDay:endDay],
                 Date = ymd(as.Date(infected$datum[beginningDay:endDay])))



png(paste0("graph_", as.Date(Sys.time()), ".png"), width = 1550, height = 980)

f <- 1.4

# First version to LinkedIn
ggplot(df2[4:(dim(df2)[1]), ], 
       aes(x = (totalConfirmed), 
           y = lead(newCases), 
           label = format(Date, "%x"))) + 
  # Main visualization
  geom_point() + 
  geom_smooth() + 
  geom_text(vjust = -0.8, hjust = 0, size = 4*f) +
  # Adjusting axes
  scale_x_log10(breaks = c(10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 2000)) +
  scale_y_log10(breaks = c(10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 2000)) +
  annotation_logticks() + 
  # Axes labs and title
  ggtitle("") + 
  xlab("Celkový počet pozitivně testovaných") + 
  ylab("Počet nově nakažených oproti předchozímu dnu") +
  # General appearance
  theme_minimal() +
  theme(axis.text=element_text(size=12*f),
        axis.title=element_text(size=16*f, face="bold")) +
  # New coordinates
  geom_segment(aes(x = 6, y = 3, xend = 3000, yend = 1500), linetype = 2) + 
  geom_text(aes(x=1250, y=750, label = "50% denní nárůst"), angle=30, size = 5*f) + 
  geom_segment(aes(x = 6, y = 1.8, xend = 3000, yend = 900), linetype = 3) + 
  geom_text(aes(x=1250, y=430, label = "30% denní nárůst"), angle=30, size = 5*f) + 
  geom_segment(aes(x = 6, y = 1.2, xend = 3000, yend = 600), linetype = 4) + 
  geom_text(aes(x=10, y=2.4, label = "20% denní nárůst"), angle=30, size = 5*f) + 
  geom_segment(aes(x = 6, y = 0.6, xend = 3000, yend = 300), linetype = 5) + 
  geom_text(aes(x=10, y=1.2, label = "10% denní nárůst"), angle=30, size = 5*f) + 
  # Countermeasures annotations
  geom_vline(aes(xintercept = 35), linetype = 2, col = "darkblue") + 
  geom_text(aes(x=35, y=80, label = "Namátkové\n kontroly \nna hranicích"), col = "darkblue", size=5*f) + 
  geom_vline(aes(xintercept = 50), linetype = 2, col = "red") + 
  geom_text(aes(x=50, y=200, label = "Zákaz akcí \ns 100+ lidmi"), col = "darkred", size=5*f) + 
  geom_vline(aes(xintercept = 75), linetype = 2, col = "darkgreen") + 
  geom_text(aes(x=75, y=400, label = "Zavřeny\n školy"), col = "darkgreen", size=5*f) + 
  geom_vline(aes(xintercept = 105), linetype = 2, col = "darkblue") + 
  geom_text(aes(x=105, y=1200, label = "Stav nouze, \nakce 30+, \n restaurace \njen ve dne"), col = "darkblue", size=5*f) + 
  geom_vline(aes(xintercept = 160), linetype = 2, col = "darkred") + 
  geom_text(aes(x=160, y=0.9, label = "Zavřeny \nobchody a \nrestaurace"), col = "darkred", size=5*f) + 
  geom_vline(aes(xintercept = 340), linetype = 2, col = "darkgreen") + 
  geom_text(aes(x=340, y=2, label = "Karanténa"), col = "darkgreen", size=5*f) + 
  geom_vline(aes(xintercept = 810), linetype = 2, col = "darkred") + 
  geom_text(aes(x=810, y=5, label = "Povinné\n roušky"), col = "darkred", size=5*f)


dev.off()
