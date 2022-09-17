library(tidyverse) 
library(maps) 
library(mapproj)
library(RColorBrewer)

#Load the Data 
load("~/desktop/SICSS-NDSU/Group Projects/Coffee Project/Data/acs2019.Rdata") 

#Subset the data to only those in coffee field
acscoffee <- subset(acs, OCCP==4150)

#Save Data
save(acscoffee, file = "~/desktop/SICSS-NDSU/Group Projects/Coffee Project/acscoffee.Rdata")

#Full-Time Insights 
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

#Create a USA Map list 
usa <- map_data('state') 

#Join the usa & meanwagefull
meanwagemergedfull <- left_join(usa, meanwagefull, by = c('region' = 'StateName')) 

#Plot Mean Wages
p <- ggplot(data=meanwagemergedfull, aes(x=long, y=lat, group=group, fill=(meanwagefull/1000))) + 
  geom_polygon(color="black") + 
  labs(labs="Mean Wage in State") + 
  scale_fill_distiller(palette = "Spectral", limits = c(min(meanwagemergedfull$meanwagefull), max(meanwagemergedfull$meanwagefull))) + 
  theme(legend.key.size = unit(2, 'cm'), legend.text = element_text(size=15)) + 
  labs(fill='Average Wage in $1,000s - FT')

p

#Plot Mean Income
p1 <- ggplot(data=meanwagemergedfull, aes(x=long, y=lat, group=group, fill=(meanincomefull/1000))) + 
  geom_polygon(color="black") + 
  labs(labs="Mean Wage in State") + 
  scale_fill_distiller(palette = "Spectral", limits = c(min(meanwagemergedfull$meanincomefull), max(meanwagemergedfull$meanincomefull))) + 
  theme(legend.key.size = unit(2, 'cm'), legend.text = element_text(size=15)) + 
  labs(fill='Average Income in $1,000s - FT')

p1

#Plot Mean Wages
p2 <- ggplot(data=meanwagemergedfull, aes(x=long, y=lat, group=group, fill=(meanhoursfull))) + 
  geom_polygon(color="black") + 
  labs(labs="Mean Wage in State") + 
  scale_fill_distiller(palette = "Spectral", limits = c(min(meanwagemergedfull$meanhoursfull), max(meanwagemergedfull$meanhoursfull))) + 
  theme(legend.key.size = unit(2, 'cm'), legend.text = element_text(size=15)) + 
  labs(fill='Average Hours Worked Per Week - FT')

p2





#Part-Time Insights 
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
meanwagemergedpart <- left_join(usa, meanwagepart, by = c('region' = 'StateName')) 

#Plot Mean Wages
p4 <- ggplot(data=meanwagemergedpart, aes(x=long, y=lat, group=group, fill=(meanwagepart/1000))) + 
  geom_polygon(color="black") + 
  labs(labs="Mean Wage in State") + 
  scale_fill_distiller(palette = "Spectral", limits = c(min(meanwagemergedpart$meanwagepart), max(meanwagemergedpart$meanwagepart))) + 
  theme(legend.key.size = unit(2, 'cm'), legend.text = element_text(size=15)) + 
  labs(fill='Average Wage in $1,000s - PT')

p4

#Plot Mean Income
p5 <- ggplot(data=meanwagemergedpart, aes(x=long, y=lat, group=group, fill=(meanincomepart/1000))) + 
  geom_polygon(color="black") + 
  labs(labs="Mean Wage in State") + 
  scale_fill_distiller(palette = "Spectral", limits = c(min(meanwagemergedpart$meanincomepart), max(meanwagemergedpart$meanincomepart))) + 
  theme(legend.key.size = unit(2, 'cm'), legend.text = element_text(size=15)) + 
  labs(fill='Average Income in $1,000s - PT')

p5

#Plot Mean Wages
p6 <- ggplot(data=meanwagemergedpart, aes(x=long, y=lat, group=group, fill=(meanhourspart))) + 
  geom_polygon(color="black") + 
  labs(labs="Mean Wage in State") + 
  scale_fill_distiller(palette = "Spectral", limits = c(min(meanwagemergedpart$meanhourspart), max(meanwagemergedpart$meanhourspart))) + 
  theme(legend.key.size = unit(2, 'cm'), legend.text = element_text(size=15)) + 
  labs(fill='Average Hours Worked Per Week - PT')

p6
