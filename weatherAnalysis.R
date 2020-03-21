library(jsonlite)
library(dplyr)
library(readr)
library(MASS)
library(httr)

setwd("C:/Users/Jindra/Documents/R/COVID19")

#GET('https://pomber.github.io/covid19/timeseries.json', write_disk(tf <- tempfile(fileext = ".json")))
#a <- fromJSON(tf)

a <- fromJSON("timeseries.json")

b <- bind_rows(a, .id = "column_label")

b.filtered <- b %>%
  filter(confirmed>100) %>%
  filter(confirmed<400)

b2 <- b.filtered %>%
  group_by(column_label) %>%
  filter(n()>2) %>%
  summarise((last(log(confirmed))-first(log(confirmed)))/(n()-1))

t <- read_csv("temperatures.csv", col_names = F)

b3 <- left_join(b2, t, by=c("column_label"="X1"))

names(b3) <- c("country", "increase", "temp")

# Without NA and Diamond Princess cruise ship
b4 <- b3[-which(b3$country=="Cruise Ship"),]
b4 <- b4[!is.na(b4$temp),]
plot(b4$temp, b4$increase)

model <- lm(increase~temp, data=b4)
summary(model)

cor(b4$temp, b4$increase)
cor.test(b4$temp, b4$increase)

rmodel <- rlm(increase~temp, data=b4)
summary(rmodel)

cov.rob(b4[,2:3], nsamp = "exact")

# Without Iran and Pakistan
b5 <- b4
b5 <- b5[-which(b5$country=="Iran"),]
b5 <- b5[-which(b5$country=="Pakistan"),]
plot(b5$temp, b5$increase)

model <- lm(increase~temp, data=b5)
summary(model)

cor(b5$temp, b5$increase)
cor.test(b5$temp, b5$increase)

rmodel <- rlm(increase~temp, data=b5)
summary(rmodel)

cov.rob(b5[,2:3], nsamp = "exact")

## GDP

GDP <- read_csv("GDP_PPP_per_capita.csv")
names(GDP) <- c("Country", "GDP_PPP", "GDP_nominal")

GDP$GDP_PPP[GDP$GDP_PPP == "N.A."] <- NA
GDP$GDP_PPP <- substr(GDP$GDP_PPP, 2, nchar(GDP$GDP_PPP))
GDP$GDP_PPP <- as.numeric(gsub(",", "", GDP$GDP_PPP))

GDP$GDP_nominal[GDP$GDP_nominal == "N.A."] <- NA
GDP$GDP_nominal <- substr(GDP$GDP_nominal, 2, nchar(GDP$GDP_nominal))
GDP$GDP_nominal <- as.numeric(gsub(",", "", GDP$GDP_nominal))

b4p <- left_join(b4, GDP, by=c("country"="Country"))
model <- lm(increase~GDP_nominal, data=b5p)
summary(model)
b5p <- left_join(b5, GDP, by=c("country"="Country"))

## Population

pop <- read_csv("population.csv")
pop <- pop[,c(1:2,5)]
names(pop) <- c("Country", "population", "density")
