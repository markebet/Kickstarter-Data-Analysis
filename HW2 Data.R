## HW 2 ##
library(dplyr)
library(mosaic)
library(lubridate)
library(ggplot2)
library(reshape)

## Loading Dataset##
crash_data = read.csv("chicago_crashes.csv")

portland_data = read.csv('PortlandWaterLevel2003.csv')


ggplot(crash_data, aes(x= factor(STREET_DIRECTION), y= factor(CRASH_HOUR)), na.rm=TRUE)
+ stat_summary_2d(fun='mean', aes(z=INJURIES_TOTAL), geom = 'title')

## Time of Day Location Crashes ##
ggplot(crash_data, aes(x= factor(STREET_DIRECTION), y= factor(CRASH_HOUR)), 
       na.rm=TRUE) +
  geom_bin2d()



## Creating the Heat Map for Question 3b ###
portland_data$Date = as_date(as.character(portland_data$Date), format="%m/%d/%Y")

Port_WL = portland_data %>%
  mutate(dd = format(as_date(Date), "%d"),
         hh = format(as.POSIXct(Time, format = "%H:%M"), "%H"),
         wd = wday(Date),
         wday = wday(Date, label = TRUE, abbr = TRUE))
  
WL_dd_avg = Port_WL %>%
  group_by(dd,hh) %>%
  summarize(avg_by_dd = sum(WL)/length(WL)) %>%
  arrange(dd,hh)  

levelplot(as.numeric(avg_by_dd) ~ as.numeric(dd)*as.numeric(hh), data = WL_dd_avg ,
          xlab = "Date", 
          ylab = "Hours",
          region = TRUE,
          main = "Date vs. Hour using Average WL")
