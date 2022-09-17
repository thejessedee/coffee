library(tidyverse) 
library(readxl) 

#Import and Clean the Data
coffee_prices <- read_excel("~/desktop/SICSS-NDSU/Group Projects/Coffee Project/Data/3b - Retail Prices.xlsx") 
coffee_imports <- read_excel("~/desktop/SICSS-NDSU/Group Projects/Coffee Project/Data/2b - Imports.xlsx")

coffee_prices <- rename(coffee_prices, country = '...1')
coffee_imports <- rename(coffee_imports, country = '...1')

coffee_prices_long <- coffee_prices %>% 
  gather(year, value, -c(country))

coffee_imports_long <- coffee_imports %>% 
  gather(year, value, -c(country))

coffee_prices_long <- rename(coffee_prices_long, prices = 'value') 

coffee_imports_long <- rename(coffee_imports_long, bags = 'value') 

#Merge the two Data Sets
coffee_long <- left_join(coffee_prices_long, coffee_imports_long) 

#Visualize 
ggplot(filter(coffee_long, country %in% c("Japan", "United States of America", "Italy")), mapping=aes(x=year, y=prices, group=country, color=country)) + 
  geom_line() + 
  xlab("Year") + 
  ylab("Price of Raw Coffee Beans") + 
  scale_color_discrete(name = "Country")


ggplot(filter(coffee_long, country %in% c("Japan", "United States of America", "Italy")), mapping=aes(x=year, y=bags, group=country, color=country)) + 
  geom_line() + 
  xlab("Year") + 
  ylab("1000s of 60-pound bags imported") + 
  scale_color_discrete(name = "Country")
