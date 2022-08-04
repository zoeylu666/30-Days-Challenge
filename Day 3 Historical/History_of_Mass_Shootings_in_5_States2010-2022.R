library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(scales)
library(ggpmisc)



data <-  read_csv("./Day 3 Historical/History_of_Mass_Shootings_in_the_USA.csv")
ls(data)

unique(data$State)

### Select the States that I am mostly interested in
data1 <- data %>% filter(State == "Illinois" | State == "California" |
                           State == "Florida" | State == "Texas" | 
                           State == "Nevada" |State == "Nevada")

##Check the date format 
unique(data1$Date)
## only keeping the year (removing the month and days)
##The date is d-m-y

data1$Date <- year(dmy(data1$Date))

##Remove unnecessary variables
data1 <- data1 %>% select(-Description, -Injured, -Total, -City)

##Group by the number of deaths by State

dat <- data1 %>% group_by(State, Date) %>% summarise(Total = sum(Dead))

dat2 <-  dat %>% filter(Date >=2010)
unique(dat$State)

#Time series plot
ggplot(data = dat2, aes(x = Date, y = Total))+
  geom_line(aes(color = State), size = 2) +
  scale_color_manual(values =c("#00AFBB", "#E7B800", "#996035", "#FFB6DB", "#D82632" )) +
  scale_x_continuous(breaks = pretty_breaks())+
  annotate(geom = "text", x=2019, y = 92, label = "Texas", hjust="left")+
  labs(title = "The Total Number of People Died of Mass Shooting From 2010 to 2022",
       subtite = "5 States Selected",
       x= "Year",
       y = "Total # of PPL died")+
  theme_minimal()


