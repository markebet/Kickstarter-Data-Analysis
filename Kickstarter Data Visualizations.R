library(tidyr)
library(dplyr)
library(vctrs)
library(magrittr)
library(mosaic)
library(lubridate)
library(ggplot2)
library(reshape)
library(ggbeeswarm)
library(GGally)
library(scales)
library(ggmosaic)
library(tibble)

kickstarter = read.csv('Kickstarter Data.csv')

## Density of Log Backers ##
ggplot(kickstarter, aes(log10(Backers))) + geom_density(adjust = 1.75)

## Density of Log USD_Pledge_Real ##
ggplot(kickstarter, aes(log10(USD_Pledged_Real))) + geom_density(alpha = .2)


## Histogram of Log Backers ##
ggplot(kickstarter, aes(log10(Backers))) + 
  geom_histogram(aes(y=..density..), color = 'black', fill = 'white')+
  geom_density(alpha = .2, fill = "#FF6666")
  
## Histogram of Log USD Pledged Real ##
ggplot(kickstarter, aes(log10(USD_Pledged_Real))) + 
  geom_histogram(aes(y=..density..), color = 'black', fill = 'white')+
  geom_density(alpha = .2, fill = "#FF6666")


## Both ##
ggplot(kickstarter, aes(log10(USD_Pledged_Real))) + geom_density() + geom_density(aes(log10(Backers)), color = 'Blue')


## Main Plot ##
d_plot = ggplot(kickstarter, aes(log10(Backers), color = 'Backers')) + geom_density(adjust = 1.75, size = 1.5) + 
  geom_density(aes(log10(USD_Pledged_Real), color = 'Pledged Amount'), size = 1.5) + 
  labs(title = 'Density Plot of Backers vs Pledged Amount', x = 'Log10 Distribution', y = 'Desnity') +
  theme(text = element_text(size = 18))

## Removing the Grid Lines ##
d_plot_m = d_plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

d_plot_m









ggplot(kickstarter, aes(log10(USD_Pledged_Real))) + geom_density() + geom_density(aes(log10(Backers
  )), color = 'Blue') + geom_density(aes(log10(USD_Goal_Real)), color = 'Red') + labs(x = 'Log10', color = 'Legend',
  title = 'Desnsity of Backers, Pledge, and Goal') 






### Mosaic Plots ###
backer = kickstarter %>% filter(Main_Category == 'Technology') %>% mutate(Main_Category = as.factor(Main_Category)) %>% 
  mutate(Launched = as.Date(Launched, "%m/%d/%y")) %>% filter(Launched > '01/01/2008')

Test = kickstarter %>% filter(Main_Category == 'Technology') %>% mutate(Main_Category = as.factor(Main_Category)) %>% mutate(Launched = as.Date(Launched)) %>% filter(Launched > '2008-01-01') 

Test3 = kickstarter %>% filter(Launched > '01/01/2008')

ggplot(backer, aes(x = year(Launched) %>% factor(), y = month(Launched) %>% factor())) +
  stat_summary_2d(fun = 'sum', aes(z = Backers), geom = 'tile') + 
  scale_fill_continuous() + labs(x = 'Year', y = 'Month', fill = "Backers",
      title = 'Total Backers per Month within Technology Category')

head(backer)




### Stacked Bar Graph - Funded vs Unfunded by Main Category
kickstart = kickstarter %>%
  mutate(Funded = if_else(USD_Pledged_Real >= USD_Goal_Real, 'Funded', 'Unfunded'))

kickstart_sumamry = kickstart %>%
  select(Main_Category) %>%
  group_by(Main_Category) %>%
  summarize(CountCategory = length(Main_Category)) %>%
  select(Main_Category, CountCategory) %>%
  arrange(CountCategory)

kickstart = left_join(kickstart, kickstart_sumamry,
                      by = c('Main_Category' = 'Main_Category'))

kickstart = kickstart %>%
  arrange(CountCategory)

zorder = kickstart_sumamry$Main_Category

plot1 = ggplot(kickstart, aes(Main_Category, level = zorder), fill = Funded)
plot1 = plot1 + geom_bar(position = position_stack(reverse = TRUE))
plot1 = plot1 + ggtitle('Main Category: Success vs Failure') + theme(plot.title = element_text(hjust = 0.5, size = 12))
plot1 = plot1 + labs(x = 'Category')
plot1 = plot1 + coord_flip()
plot1 = plot1 + scale_fill_manual(values = c('Green', 'Red'))
plot1


## Boxplot: Main Category vs Pledged Amount ###
sorder = kickstarter %>%
  select(Main_Category, USD_Pledged_Real) %>%
  group_by(Main_Category) %>%
  summarize(Median = median(USD_Pledged_Real)) %>%
  arrange(Median) %>%
  select(Main_Category)

plot2 = ggplot(kickstarter, aes(x=factor(Main_Category, level = sorder$Main_Category), y= USD_Pledged_Real)) + geom_boxplot()
plot2 = plot2 + scale_y_log10()
plot2 = plot2 + scale_y_log10(breaks = 10^(1:10), labels = trans_format("log10", math_format(10^.x)))
plot2 = plot2 + ggtitle("Main Category vs Pledged Amount Distribution")+theme(plot.title = element_text(hjust = 0.5, size = 12))
plot2 = plot2 + labs(x = 'Category', y = 'Amount')
plot2 = plot2 + coord_flip()
plot2
