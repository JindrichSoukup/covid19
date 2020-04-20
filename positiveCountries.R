library(ggrepel)
library(ggplot)
library(dplyr)

GET('https://pomber.github.io/covid19/timeseries.json', write_disk(tf <- tempfile(fileext = ".json")))
a <- fromJSON(tf)

df <- bind_rows(a, .id = "Country")

df$date <- as.Date(df$date)
df$active <- df$confirmed - df$deaths - df$recovered
df$diff <- c(0, diff(df$confirmed))

df <- df %>%
  group_by(Country) %>%
  mutate(diff7 = diff + lag(diff) + lag(diff, 2) + lag(diff, 3) + lag(diff, 4) + lag(diff, 5) + lag(diff, 6)) %>%
  ungroup()

countries <- c("Czechia", "Austria", "Denmark", "France", "Finland", "Germany", "Greece", "Iran", "Israel", 
               "Japan", "Italy", "Netherlands", "Poland", "Russia", "US", "Singapore", "Slovakia", "Spain",
               "Turkey", "United Kingdom", "Switzerland", "China", "Korea, South")

countries_up <- c("Denmark", "France", "Finland", "Israel",
               "Japan", "Russia", "Singapore", "Poland","Germany", 
               "Turkey", "United Kingdom", "Switzerland", "US")

countries_down <- c("Czechia", "Austria", "Greece", "Iran", 
               "Italy", "Netherlands", "Slovakia",  "Spain",
               "China", "Korea, South")

df2 <- df %>%
  filter(Country %in% countries) %>%
  filter(date != 2020-01-22)

down <- df2 %>% group_by(Country) %>% summarise(down = last(active)<max(active))
down_all <- df %>% group_by(Country) %>% summarise(down = last(active)<max(active))

df2 <- left_join(df2, down)

df2today <- df2[df2$date==max(df$date),]

#ggplot(df2, aes(x = active, y = diff7, color = as.factor(Country))) + 
#ggplot(df2, aes(x = active, y = diff7, color = ifelse(Country != "Czechia", "black", "darkred"))) + 
ggplot(df2, aes(x = active, y = diff7, color = down)) + 
  geom_segment(aes(xend = c(tail(active, n = -1), NA), yend = c(tail(diff7, n = -1), NA))) + 
  annotate("point", 
           x = df2today$active, 
           y = df2today$diff7) + 
  geom_text_repel(data = df2today[df2today$Country %in% countries_up, ],
  #geom_text_repel(data = df2today[!df2today$down, ],
                  aes(x = active, 
                      y = diff7, 
                      label = Country),
                  nudge_x = -0.4,
                  nudge_y = 0.4) + 
  geom_text_repel(data = df2today[df2today$Country %in% countries_down, ],
  #geom_text_repel(data = df2today[df2today$down, ],
                  aes(x = active, 
                      y = diff7, 
                      label = Country),
                  nudge_x = 0.2,
                  nudge_y = -0.2) + 
  scale_color_manual(values=c("darkred", "darkgreen")) +
  scale_x_log10(limits = c(100,500000)) + 
  scale_y_log10(limits = c(100,500000)) + 
  theme(legend.position = "none") + 
  theme_minimal() + 
  xlab("Počet AKTUÁLNĚ známých nakažených (logaritmická osa)") + 
  ylab("Noví známí nakažení za posledních 7 dnů (logaritmická osa)") +
  ggtitle("Aktuální počet nemocných je nižší (zelená) / vyšší (červená) než bylo maximum") + 
  theme(legend.position = "none")
  




## Only positive countries

down_all <- df %>% 
  group_by(Country) %>% 
  summarise(down = last(active) < max(active))

down_big_enough <- c("China", "Korea, South", "Switzerland", "Austria", "Australia", "Malaysia", "Greece", "Bosnia and Herzegovina", "Slovenia", "Latvia",
                     "Germany", "Iceland", "Norway", "Vietnam", "Germany", "Iran", "Algeria", "New Zealand", "Iraq", "Armenia", "Lithuania",
                     "South Africa", "Croatia", "Denmark", "Mexico", "Slovakia", "Czechia", "Brazil", "France", "Spain", "Israel")

df2 <- df %>%
  filter(Country %in% down_all$Country[down_all$down]) %>%
  filter(Country %in% down_big_enough) %>%
  filter(date != 2020-01-22)

df2 <- left_join(df2, down_all)

df2today <- df2[df2$date==max(df$date),]

#ggplot(df2, aes(x = active, y = diff7, color = as.factor(Country))) + 
#ggplot(df2, aes(x = active, y = diff7, color = ifelse(Country != "Czechia", "black", "darkred"))) + 
ggplot(df2, aes(x = active, y = diff7, color = down)) + 
  geom_segment(aes(xend = c(tail(active, n = -1), NA), yend = c(tail(diff7, n = -1), NA))) + 
  annotate("point", 
           x = df2today$active, 
           y = df2today$diff7) + 
  geom_text_repel(data = df2today,
                  #geom_text_repel(data = df2today[!df2today$down, ],
                  aes(x = active, 
                      y = diff7, 
                      label = Country)) + 
  scale_color_manual(values=c("darkgreen", "darkgreen")) +
  scale_x_log10(limits = c(100, 100000)) + 
  scale_y_log10(limits = c(100, 93000)) + 
  theme(legend.position = "none") + 
  theme_minimal() + 
  xlab("Počet známých nakažených MÍNUS POČET UZDRAVENÝCH A ZEMŘELÝCH (logaritmická osa)") + 
  ylab("Noví známí nakažení za posledních 7 dnů (logaritmická osa)") +
  ggtitle("Země, kde je aktuální počet známých nemocných nižší než bylo maximum") + 
  theme(legend.position = "none")


## Countries with actual number infected bellow maximum

days <- length(unique(df$date))

positive <- c()

for(i in c(2:days)){
  res <- df %>% 
    filter(date < df$date[i]) %>%
    group_by(Country) %>% 
    summarise(down = last(active) < max(active)) %>%
    select(down) %>%
    mutate(down = ifelse(down,1,0)) %>%
    sum()
  positive <- c(positive, res)
}

pos <- data.frame(date = format(unique(df$date), "%m/%d"), positive = positive/length(unique(df$Country)))

#plot(positive~date, data = pos[55:days,])

pos <- data.frame(date = format(unique(df$date), "%d. %m."), positive = positive/length(unique(df$Country)))
pos2 <- pos[55:days, ]
pos2$date <- as.Date(pos2$date, format = "%d. %m.")

ggplot(pos2, aes(x = date, y = positive*100)) + 
  geom_point() + 
  geom_smooth() +
  xlab("Date") + 
  ylab("% of counties") + 
  ylim(c(0, 100)) + 
  ggtitle("Percentage of countries, where actual number of infected is below maximal state") + 
  geom_hline(yintercept = 0) + 
  geom_hline(yintercept = 100) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(pos2, aes(x = date, y = positive*100)) + 
  geom_point() + 
  geom_smooth() + 
  xlab("Datum") + 
  ylab("%") + 
  ylim(c(0, 100)) + 
  ggtitle("Podíl zemí, kde aktuální počet nemocných je nižší než bylo maximum") + 
  geom_hline(yintercept = 0) + 
  geom_hline(yintercept = 100) + 
  theme_minimal() + 
  scale_x_date(date_labels = "%d. %m.", breaks=pos2$date) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Countries, where in the number of infected decreased

df <- df %>%
  group_by(Country) %>%
  mutate(activediff = c(0, diff(active))) %>%
  ungroup

decr <- df %>%
  group_by(date) %>%
  summarize(n = 100*sum(activediff<0)/n())

decr2 <- decr[55:days, ]

decr2$date2 <- gsub(" 0", " ", format(decr2$date, " %d. %m."))
decr2$date2 <- factor(decr2$date2, levels = decr2$date2)

ggplot(decr2, aes(x = date, y = n)) + 
  geom_smooth(se = F, color = "darkgrey") + 
  geom_point(color = "darkred") + 
  ggtitle("Podíl zemí, kde daný den klesl počet nakažených") + 
  ylab("") +
  xlab("") + 
  theme_minimal(base_family = "PT Serif") + 
  scale_x_date(breaks = decr2$date[seq(7, dim(decr2)[1], 7)], 
               minor_breaks = c(),
               labels = function(x) gsub(" 0", " ", format(x, "%a %d. %m."))
               ) +
  scale_y_continuous(labels = function(x) paste0(x, " %"), limits = c(0, 100)) + 
  theme(plot.title = element_text(hjust = 0.5))

# https://stackoverflow.com/questions/34522732/changing-fonts-in-ggplot2
# https://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html
# https://www.andrewheiss.com/blog/2017/09/27/working-with-r-cairo-graphics-custom-fonts-and-ggplot/
# https://www.statworx.com/at/blog/customizing-time-and-date-scales-in-ggplot2/
library(extrafont)

#font_import()
#font_install("fontcm")
#loadfonts(device = "win")

#fonts()
#fonttable()

## Fit by general logistic function gives very high carrying capacity ----

#library(growthcurver)
#SummarizeGrowth(as.numeric(decr2$date)-18340, decr2$n)
# K = 34.41

## Fit by (generalized) logistic curve with fixed limiting capacity ----

library(growthrates)

fit <- fit_growthmodel(FUN = grow_logistic, 
                             p = c(y0 = 0.05, mumax = 1, K = 100), 
                             time = as.numeric(decr2$date)-18336, 
                             y = decr2$n, 
                             lower = c(y0 = 0, mumax = 0, K = 99), 
                             upper = c(y0 = 5, mumax = Inf, K = 101), 
)
summary(fit)

time <- seq(0, 100, 1)

# Use to update the prediction
coefFit <- coef(fit)

# Stored coef from Apr18 prediction
coefFit <- c(y0 = 2.208, mumax = 0.07943, K = 99)

y <- grow_logistic(time, coef(fit))[,"y"]
#plot(time, y, type = "l")
#as.Date(which(y>50)[1], origin = "2020-03-16")
#as.Date(which(y>90)[1], origin = "2020-03-16")

decr3 <- data.frame(date = as.Date(time, origin = "2020-03-16"), y = y)

ggplot(decr2, aes(x = date, y = n)) + 
  geom_smooth(se = F, color = "darkgrey") + 
  geom_point(color = "darkred") + 
  ggtitle("Podíl zemí, kde daný den klesl počet nakažených, logistická predikce udělaná 19.4.") + 
  ylab("") +
  xlab("") + 
  theme_minimal(base_family = "PT Serif") + 
  scale_x_date(breaks = decr3$date[seq(7, dim(decr3)[1], 7)], 
               minor_breaks = c(),
               labels = function(x) gsub(" 0", " ", format(x, "%a %d. %m."))
  ) +
  scale_y_continuous(labels = function(x) paste0(x, " %"), limits = c(0, 100)) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_line(data = decr3, aes(x = date, y = y), col = "darkgreen")



fit2 <- fit_growthmodel(FUN = grow_genlogistic, 
                        p = c(y0 = 3, mumax = 0.1, K = 100, alpha = 1, beta = 1, gamma = 1), 
                        time = as.numeric(decr2$date)-18336, 
                        y = decr2$n, 
                        lower = c(y0 = 0.1, mumax = 0.0001, K = 99, alpha = 0.01, beta = 0.1, gamma = 1), 
                        upper = c(y0 = 3.1, mumax = 0.5, K = 101, alpha = 10, beta = 5, gamma = 1.1),
                        method = "L-BFGS-B"
)
summary(fit2)

time <- seq(0, 100, 1)

# Enable to actualize prediction
#coefFit2 <- coef(fit2)

# Use for prediction from Apr18
coefFit2 <- c(y0 = 1.824, mumax = 0.1257, K = 99.68, alpha = 0.8697, beta = 0.7192, gamma = 1.0741)

y <- grow_genlogistic(time, coefFit2)[,"y"]
#plot(time, y, type = "l")
#as.Date(which(y>50)[1], origin = "2020-03-16")
#as.Date(which(y>90)[1], origin = "2020-03-16")

decr4 <- data.frame(date = as.Date(time, origin = "2020-03-16"), y = y)

ggplot(decr2, aes(x = date, y = n)) + 
  geom_smooth(se = F, color = "darkgrey") + 
  geom_point(color = "darkred") + 
  ggtitle("Podíl zemí, kde daný den klesl počet nakažených, genlogistic predikce (zeleně) udělaná 19.4.") + 
  ylab("") +
  xlab("") + 
  theme_minimal(base_family = "PT Serif") + 
  scale_x_date(breaks = decr4$date[seq(7, dim(decr4)[1], 7)], 
               minor_breaks = c(),
               labels = function(x) gsub(" 0", " ", format(x, "%a %d. %m."))
  ) +
  scale_y_continuous(labels = function(x) paste0(x, " %"), limits = c(0, 100)) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_line(data = decr4, aes(x = date, y = y), col = "darkgreen")
