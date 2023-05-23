library(dplyr)
library(mosaic)
library(lubridate)
library(ggplot2)
library(reshape)

## Loading Dataset##
intel_data = read.csv("Intel-1998.csv")

## Plotting Line Graph ##
date_only = as.Date(intel_data$Date, "%m/%d/%Y") #convert date  
intel_data_2 = cbind(intel_data, date_only) #append to new data frame
ggplot(data = intel_data_2, aes(x= date_only, y= Close)) + geom_line()

##Plotting Scatterplot##
price_range = intel_data_2$High - intel_data_2$Low
intel_data_2 = cbind(intel_data_2, price_range)

ggplot(data = intel_data_2, aes(x = Volume, y = price_range)) +
  geom_point()


##Question 5###
cellPlans = data.frame(
  c("ATT", "Sprint", "Verizon", "ATT", "Sprint",
    "Verizon", "ATT", "Sprint", "Verizon", "ATT",
    "Verizon", "Sprint", "Verizon", "ATT",
    "Verizon", "Sprint", "ATT", "ATT", "Sprint"),
  c(1, 1, 2, 3, 3, 4, 6, 6, 8, 10, 12, 12, 16, 16,
    24, 24, 25, 30, 40),
  c(30, 20, 35, 40, 30, 50, 60, 45, 70, 80, 80, 60,
    90, 90, 110, 80, 110, 135, 100))
names(cellPlans) = c("Company", "DataGB", "Price")

ggplot(data = cellPlans, aes(x = DataGB, y = Price, color = Company,)) +
  geom_point()

##Stacked Graph## 
ggplot(data = cellPlans, aes(x = DataGB, y = Price, fill = Company)) + geom_bar(stat='identity', position = position_dodge()) + 
  geom_text(aes(label = Price), vjust = 1.6, color = 'black', size = 3.5) + theme_minimal() 

### Line Graph##
ggplot(data = cellPlans, aes(x = DataGB, y = Price, color = Company)) + geom_line()

ggplot( data=cellPlans, aes(x=DataGB, fill=Company) ) + stat_density(alpha=.5)

ggplot( data=cellPlans, aes(x=DataGB, y=Price, color=Company) ) + geom_point()
 