library(tidyr)
library(dplyr)
library(vctrs)
library(magrittr)
library(mosaic)
library(lubridate)
library(ggplot2)
library(reshape)
library(ggbeeswarm)

## Loading the dataset ##
d = read.csv("MessierData_2.csv")

plot = ggplot(data = d, aes(x = factor(Kind), y = log10(Distance..LY.))) +
  geom_beeswarm() + labs(title = "Beeswarm of Kind vs Log10(Distance)", x='Kind',
  y = 'log10(Distance)')

plot

## Scatter Plot## 
plot2 = ggplot(data = d, aes(x = log10(Distance..LY.), y = Apparent.Magnitude)) +
  geom_point(size = 1.4, color = 'navyblue')

plot2 + ggtitle("Scatterplot of Log10(Distance) vs Apparent Magnitude")

### Amend the Scatterplot ##
plot3 = ggplot(d, aes(x = log10(Distance..LY.), y = Apparent.Magnitude)) + 
  geom_point(size = d$Size, color = 'black', fill = "lightskyblue", alpha = 0.35,
             shape = 21, stroke = 1.5)

plot3 + ggtitle("Scatterplot of Log10(Distance) vs Apparent Magnitude with Angular Size")


## Extra Credit for Q4 ##
remove.packages("vctrs")
install.packages("vctrs") 


aq = read.csv("AirQuality.csv")
head(aq)
which(is.na(aq))
lapply(aq,function(x){length(which(is.na(x)))})
aq = aq %>% fill(Solar.R)
     
aq1 = aq %$% data.frame(wind = sort(Wind), solar = sort(Solar.R))

aq1 %>% 
  mutate(wind = (wind-min(wind)) / (max(wind) - min(wind))) %>%
  mutate(solar = (solar-min(solar)) / (max(solar) - min(solar))) %>%
  ggplot(aes(wind,solar)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  labs(title = "QQ Plot of Solar.R vs Wind")
