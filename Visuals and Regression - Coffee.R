#####Libraries#####
library(tidyverse)
library(rvest) 
library(usmap)
library(jtools) #summ and export_summs 
library(readxl) 

#####Data Importation and Collection#####
url <- "https://vividmaps.com/us-coffee-index/"
url %>%
  read_html() #read HTML into R
css_selector <- "#tablepress-153" #using inspection tool

table2 <- url %>% 
  read_html() %>% 
  html_element(css = css_selector) %>% 
  html_table()

#Rename columns
table2 <- rename(table2, Shops = 'Coffe Shops per 100k')
table2 <- rename(table2, state = "State")
table2 <- rename(table2, Avg_Price = 'Avg Coffe Price')

#Recode States
table2$state <- tolower(table2$state)

#Load the Data 
load("~/desktop/SICSS-NDSU/Group Projects/Coffee Project/Data/acscoffee.Rdata") 
  #To access the ACS data follow this link: https://www2.census.gov/programs-surveys/acs/data/pums/2019/1-Year/
  #To get this dataset we limited our responses to those in the "Coffee" categories (OCCP == 4150)

#####Subsetting Full-Time##### 
fullcoffee <- subset(acscoffee, OCCP==4150 & WKHP>39) 

#Get mean Wage for FT people
meanwagefull <- fullcoffee %>%
  group_by(ST) %>%
  summarise_at(vars(WAGP), list(meanwagefull = mean))

#Get mean Hours for FT people 
meanhoursfull <- fullcoffee %>%
  group_by(ST) %>%
  summarise_at(vars(WKHP), list(meanhoursfull = mean))

#Create Mean Income Per State 
meanincomefull <- fullcoffee %>%
  group_by(ST) %>%
  summarise_at(vars(PINCP), list(meanincomefull = mean))

#Add a field for State Name 
meanwagefull <- meanwagefull %>% 
  mutate(StateName = 
           case_when( 
             ST == '01' ~ "alabama",
             ST == '02' ~ "alaska", 
             ST == '04' ~ "arizona", 
             ST == '05' ~ "arkansas", 
             ST == '06' ~ "california", 
             ST == '08' ~ "colorado", 
             ST == '09' ~ "connecticut", 
             ST == '10' ~ "delaware", 
             ST == '12' ~ "florida", 
             ST == '13' ~ "georgia", 
             ST == '15' ~ "hawaii", 
             ST == '16' ~ "idaho", 
             ST == '17' ~ "illinois", 
             ST == '18' ~ "indiana", 
             ST == '19' ~ "iowa", 
             ST == '20' ~ "kansas", 
             ST == '21' ~ "kentucky", 
             ST == '22' ~ "louisiana", 
             ST == '23' ~ "maine", 
             ST == '24' ~ "maryland", 
             ST == '25' ~ "massachusetts", 
             ST == '26' ~ "michigan", 
             ST == '27' ~ "minnesota", 
             ST == '28' ~ "mississippi", 
             ST == '29' ~ "missouri", 
             ST == '30' ~ "montana", 
             ST == '31' ~ "nebraska", 
             ST == '32' ~ "nevada", 
             ST == '33' ~ "new hampshire", 
             ST == '34' ~ "new jersey", 
             ST == '35' ~ "new mexico",
             ST == '36' ~ "new york", 
             ST == '37' ~ "north carolina", 
             ST == '38' ~ "north dakota", 
             ST == '39' ~ "ohio", 
             ST == '40' ~ "oklahoma", 
             ST == '41' ~ "oregon", 
             ST == '42' ~ "pennsylvania", 
             ST == '44' ~ "rhode island", 
             ST == '45' ~ "south carolina", 
             ST == '46' ~ "south dakota", 
             ST == '47' ~ "tennessee", 
             ST == '48' ~ "texas", 
             ST == '49' ~ "utah", 
             ST == '50' ~ "vermont", 
             ST == '51' ~ "virginia", 
             ST == '53' ~ "washington", 
             ST == '54' ~ "west virginia", 
             ST == '55' ~ "wisconsin", 
             ST == '56' ~ "wyoming"))

#Join Together Averages
meanwagefull <- left_join(meanwagefull, meanhoursfull) 
meanwagefull <- left_join(meanwagefull, meanincomefull) 

#Join the table2 & meanwagefull
meanwagemergedfull <- left_join(table2, meanwagefull, by = c('state' = 'StateName')) 


#####Subsetting Part-Time##### 
partcoffee <- subset(acscoffee, OCCP==4150 & WKHP<40) 

#Get mean Wage for FT people
meanwagepart <- partcoffee %>%
  group_by(ST) %>%
  summarise_at(vars(WAGP), list(meanwagepart = mean))

#Get mean Hours for FT people 
meanhourspart <- partcoffee %>%
  group_by(ST) %>%
  summarise_at(vars(WKHP), list(meanhourspart = mean))

#Create Mean Income Per State 
meanincomepart <- partcoffee %>%
  group_by(ST) %>%
  summarise_at(vars(PINCP), list(meanincomepart = mean))

#Add a field for State Name 
meanwagepart <- meanwagepart %>% 
  mutate(StateName = 
           case_when( 
             ST == '01' ~ "alabama",
             ST == '02' ~ "alaska", 
             ST == '04' ~ "arizona", 
             ST == '05' ~ "arkansas", 
             ST == '06' ~ "california", 
             ST == '08' ~ "colorado", 
             ST == '09' ~ "connecticut", 
             ST == '10' ~ "delaware", 
             ST == '12' ~ "florida", 
             ST == '13' ~ "georgia", 
             ST == '15' ~ "hawaii", 
             ST == '16' ~ "idaho", 
             ST == '17' ~ "illinois", 
             ST == '18' ~ "indiana", 
             ST == '19' ~ "iowa", 
             ST == '20' ~ "kansas", 
             ST == '21' ~ "kentucky", 
             ST == '22' ~ "louisiana", 
             ST == '23' ~ "maine", 
             ST == '24' ~ "maryland", 
             ST == '25' ~ "massachusetts", 
             ST == '26' ~ "michigan", 
             ST == '27' ~ "minnesota", 
             ST == '28' ~ "mississippi", 
             ST == '29' ~ "missouri", 
             ST == '30' ~ "montana", 
             ST == '31' ~ "nebraska", 
             ST == '32' ~ "nevada", 
             ST == '33' ~ "new hampshire", 
             ST == '34' ~ "new jersey", 
             ST == '35' ~ "new mexico",
             ST == '36' ~ "new york", 
             ST == '37' ~ "north carolina", 
             ST == '38' ~ "north dakota", 
             ST == '39' ~ "ohio", 
             ST == '40' ~ "oklahoma", 
             ST == '41' ~ "oregon", 
             ST == '42' ~ "pennsylvania", 
             ST == '44' ~ "rhode island", 
             ST == '45' ~ "south carolina", 
             ST == '46' ~ "south dakota", 
             ST == '47' ~ "tennessee", 
             ST == '48' ~ "texas", 
             ST == '49' ~ "utah", 
             ST == '50' ~ "vermont", 
             ST == '51' ~ "virginia", 
             ST == '53' ~ "washington", 
             ST == '54' ~ "west virginia", 
             ST == '55' ~ "wisconsin", 
             ST == '56' ~ "wyoming"))

#Join Together Averages
meanwagepart <- left_join(meanwagepart, meanhourspart) 
meanwagepart <- left_join(meanwagepart, meanincomepart) 

#Join the usa & meanwagefull
meanwagemergedpart <- left_join(table2, meanwagepart, by = c('state' = 'StateName')) 



#####Visualization#####
#create map for nr. of coffee shops per state
map1 <- plot_usmap(
  regions = "state",
  labels = "TRUE",
  label_color = "black",
  data = table2,
  values = "Shops",
  color = "burlywood4"
) + 
  scale_fill_continuous(low = "white", high = "burlywood4", name = "Number of Shops per 100k People", label = scales::comma) + 
  labs(title = "Number of Coffee Shops in the US")+ 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "right")
map1
 
#create map for average price of coffee per state
map2 <- plot_usmap(
  regions = "state",
  labels = "TRUE",
  label_color = "black",
  data = table2,
  values = "Avg_Price",
  color = "darkseagreen"
) + 
  scale_fill_continuous(low = "white", high = "darkseagreen4", name = "Price in Dollars", label = scales::comma) + 
  labs(title = "Average Price of Coffee in the US")+ 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "right")
map2


#Plot Mean Wages - FT
map3 <- plot_usmap(
  regions = "state",
  labels = "TRUE",
  label_color = "black",
  data = meanwagemergedfull,
  values = "meanwagefull",
  color = "burlywood4"
) + 
  scale_fill_continuous(low = "white", high = "burlywood4", name = "Mean Wage - FT", label = scales::comma) + 
  labs(title = "Mean Wage of Full-Time Coffee Workers")+ 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "right")
map3

#Plot Mean Income - FT
map4 <- plot_usmap(
  regions = "state",
  labels = "TRUE",
  label_color = "black",
  data = meanwagemergedfull,
  values = "meanincomefull",
  color = "burlywood4"
) + 
  scale_fill_continuous(low = "white", high = "burlywood4", name = "Mean Income - FT", label = scales::comma) + 
  labs(title = "Mean Income of Full-Time Coffee Workers")+ 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "right")
map4

#Plot Mean Hours Worked Per Week - FT
map5 <- plot_usmap(
  regions = "state",
  labels = "TRUE",
  label_color = "black",
  data = meanwagemergedfull,
  values = "meanhoursfull",
  color = "burlywood4"
) + 
  scale_fill_continuous(low = "white", high = "burlywood4", name = "Mean Hours - FT", label = scales::comma) + 
  labs(title = "Mean Hours Worked Per Week of Full-Time Coffee Workers")+ 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "right")
map5



#Plot Mean Wages - PT
map6 <- plot_usmap(
  regions = "state",
  labels = "TRUE",
  label_color = "black",
  data = meanwagemergedpart,
  values = "meanwagepart",
  color = "burlywood4"
) + 
  scale_fill_continuous(low = "white", high = "burlywood4", name = "Mean Wage - PT", label = scales::comma) + 
  labs(title = "Mean Wage of Part-Time Coffee Workers")+ 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "right")
map6

#Plot Mean Income - PT
map7 <- plot_usmap(
  regions = "state",
  labels = "TRUE",
  label_color = "black",
  data = meanwagemergedpart,
  values = "meanincomepart",
  color = "burlywood4"
) + 
  scale_fill_continuous(low = "white", high = "burlywood4", name = "Mean Income - PT", label = scales::comma) + 
  labs(title = "Mean Income of Part-Time Coffee Workers")+ 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "right")
map7

#Plot Mean Hours Worked Per Week - PT
map8 <- plot_usmap(
  regions = "state",
  labels = "TRUE",
  label_color = "black",
  data = meanwagemergedpart,
  values = "meanhourspart",
  color = "burlywood4"
) + 
  scale_fill_continuous(low = "white", high = "burlywood4", name = "Mean Hours - PT", label = scales::comma) + 
  labs(title = "Mean Hours Worked Per Week of Part-Time Coffee Workers")+ 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "right")
map8



#####Regressions#####
#FULL TIME
#Wage ~ Shops
reg1a <- lm(meanwagefull ~ Shops, data = meanwagemergedfull, na.rm=TRUE) 
summary(reg1a) 

reg1b <- lm(meanwagefull ~ Avg_Price, data = meanwagemergedfull, na.rm=TRUE) 
summary(reg1b) 

reg1c <- lm(meanwagefull ~ Shops + Avg_Price, data = meanwagemergedfull, na.rm=TRUE) 
summary(reg1c) 

reg1d <- lm(meanwagefull ~ Shops*Avg_Price + Shops + Avg_Price, data = meanwagemergedfull, na.rm=TRUE)
summary(reg1d) 

export_summs(reg1a, reg1b, reg1c, reg1d, coefs = c("Shops per 100k" = "Shops", "Average Price of Coffee" = "Avg_Price", "Shops * Avg Price" = "Shops:Avg_Price", "Intercept" = "(Intercept)"), to.file="xlsx", file.name = "~/desktop/Mean Wage - FT.xlsx")  #Think outreg2 

#Income ~ Shops
reg2a <- lm(meanincomefull ~ Shops, data = meanwagemergedfull, na.rm=TRUE) 
summary(reg2a) 

reg2b <- lm(meanincomefull ~ Avg_Price, data = meanwagemergedfull, na.rm=TRUE) 
summary(reg2b) 

reg2c <- lm(meanincomefull ~ Shops + Avg_Price, data = meanwagemergedfull, na.rm=TRUE) 
summary(reg2c) 

reg2d <- lm(meanincomefull ~ Shops*Avg_Price + Shops + Avg_Price, data = meanwagemergedfull, na.rm=TRUE)
summary(reg2d) 

export_summs(reg2a, reg2b, reg2c, reg2d, coefs = c("Shops per 100k" = "Shops", "Average Price of Coffee" = "Avg_Price", "Shops * Avg Price" = "Shops:Avg_Price", "Intercept" = "(Intercept)"), to.file="xlsx", file.name = "~/desktop/Mean Income - FT.xlsx")  #Think outreg2 

#Hours ~ Shops
reg3a <- lm(meanhoursfull ~ Shops, data = meanwagemergedfull, na.rm=TRUE) 
summary(reg3a) 

reg3b <- lm(meanhoursfull ~ Avg_Price, data = meanwagemergedfull, na.rm=TRUE) 
summary(reg3b) 

reg3c <- lm(meanhoursfull ~ Shops + Avg_Price, data = meanwagemergedfull, na.rm=TRUE) 
summary(reg3c) 

reg3d <- lm(meanhoursfull ~ Shops*Avg_Price + Shops + Avg_Price, data = meanwagemergedfull, na.rm=TRUE)
summary(reg3d) 

export_summs(reg3a, reg3b, reg3c, reg3d, coefs = c("Shops per 100k" = "Shops", "Average Price of Coffee" = "Avg_Price", "Shops * Avg Price" = "Shops:Avg_Price", "Intercept" = "(Intercept)"), to.file="xlsx", file.name = "~/desktop/Mean Hours - FT.xlsx")  #Think outreg2 


#PART TIME
#Wage ~ Shops
reg4a <- lm(meanwagepart ~ Shops, data = meanwagemergedpart, na.rm=TRUE) 
summary(reg4a) 

reg4b <- lm(meanwagepart ~ Avg_Price, data = meanwagemergedpart, na.rm=TRUE) 
summary(reg4b) 

reg4c <- lm(meanwagepart ~ Shops + Avg_Price, data = meanwagemergedpart, na.rm=TRUE) 
summary(reg4c) 

reg4d <- lm(meanwagepart ~ Shops*Avg_Price + Shops + Avg_Price, data = meanwagemergedpart, na.rm=TRUE)
summary(reg4d) 

export_summs(reg4a, reg4b, reg4c, reg4d, coefs = c("Shops per 100k" = "Shops", "Average Price of Coffee" = "Avg_Price", "Shops * Avg Price" = "Shops:Avg_Price", "Intercept" = "(Intercept)"), to.file="xlsx", file.name = "~/desktop/Mean Wage - PT.xlsx")  #Think outreg2 

#Income ~ Shops
reg5a <- lm(meanincomepart ~ Shops, data = meanwagemergedpart, na.rm=TRUE) 
summary(reg5a) 

reg5b <- lm(meanincomepart ~ Avg_Price, data = meanwagemergedpart, na.rm=TRUE) 
summary(reg5b) 

reg5c <- lm(meanincomepart ~ Shops + Avg_Price, data = meanwagemergedpart, na.rm=TRUE) 
summary(reg5c) 

reg5d <- lm(meanincomepart ~ Shops*Avg_Price + Shops + Avg_Price, data = meanwagemergedpart, na.rm=TRUE)
summary(reg5d) 

export_summs(reg5a, reg5b, reg5c, reg5d, coefs = c("Shops per 100k" = "Shops", "Average Price of Coffee" = "Avg_Price", "Shops * Avg Price" = "Shops:Avg_Price", "Intercept" = "(Intercept)"), to.file="xlsx", file.name = "~/desktop/Mean Income - PT.xlsx")  #Think outreg2 

#Hours ~ Shops
reg6a <- lm(meanhourspart ~ Shops, data = meanwagemergedpart, na.rm=TRUE) 
summary(reg6a) 

reg6b <- lm(meanhourspart ~ Avg_Price, data = meanwagemergedpart, na.rm=TRUE) 
summary(reg6b) 

reg6c <- lm(meanhourspart ~ Shops + Avg_Price, data = meanwagemergedpart, na.rm=TRUE) 
summary(reg6c) 

reg6d <- lm(meanhourspart ~ Shops*Avg_Price + Shops + Avg_Price, data = meanwagemergedpart, na.rm=TRUE)
summary(reg6d) 

export_summs(reg6a, reg6b, reg6c, reg6d, coefs = c("Shops per 100k" = "Shops", "Average Price of Coffee" = "Avg_Price", "Shops * Avg Price" = "Shops:Avg_Price", "Intercept" = "(Intercept)"), to.file="xlsx", file.name = "~/desktop/Mean Hours - PT.xlsx")  #Think outreg2 

#####Coffee Over Time#####
#Data can be downloaded from: https://ico.org/new_historical.asp
#You will need to "clean" the excel files and remove extra rows and such... 
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
