install.packages(c('readr','here','dplyr','janitor','lubridate',"tidyverse",'ggplot2')) #to install the packages

library(readr) # To read our .csv data
library(here) # To know the project path
library(dplyr) # to manipulate data
library(janitor) #To make all the columns names snake_case
library(lubridate) #modify dates
library(tidyverse)
library(ggplot2)

# reading data of arab barometer -----------------------------------------------

print(getwd())
arab_data <- read.csv(here("Arab_Barometer.csv"))
arab_data <- clean_names(arab_data) 




arab_data_core <- arab_data |>
  rename_with(.fn = tolower,.col=everything())|>
  filter(country==7) |>
  select(country,q1001,q1002)|>
  rename(q1001, age=q1001)|>
  rename(q1002, gender=q1002)|>
  mutate(country=case_when(country==7~"iraq"))|>
  mutate(gender = case_when(gender==1~"male",gender==2~"female"))|>
  View()

  #  group_by(gender)|>summarise(genavg=mean(age))|>

arab_data_ir <- arab_data |>
  rename_with(.fn = tolower,.col=everything())|>
  filter(country==7)|> 
  select(q732b,q730b,q734a,q734b,q104,q104a_1)|>
  mutate(q734b=case_when(q734b== 1 ~ 'Strongly favor',
                         q734b== 2 ~ 'Favor',
                         q734b== 3 ~ 'Oppose',
                         q734b== 4 ~ 'Strongly oppose',
                         q734b== 98 ~ 'Don’t know',
                         q734b== 99 ~ 'Refused to answer',
                         T ~ 'na'))|>
  rename(q734b, 'relations between Morocco and Israel'=q734b)|>
  
  mutate(q104=case_when(q104== 1 ~ 'yes',
                        q104== 2 ~ 'no',
                        q104== 98 ~ 'Don’t know',
                        q104== 99 ~ 'Refused to answer',
                        T ~ 'na'))|>
  rename(q104, 'thought about emigrating from your country'=q104)|>
  
  
  mutate(q104a_1=case_when(q104a_1== 1 ~ 'Strongly favor',
                           q104a_1== 2 ~ 'Favor',
                           q104a_1== 3 ~ 'Oppose',
                           q104a_1== 4 ~ 'Strongly oppose',
                           q104a_1== 98 ~ 'Don’t know',
                           q104a_1== 99 ~ 'Refused to answer',
                           T ~ 'na'))|>
  rename(q104a_1, 'Why have you thought about emigrating?'=q104a_1)|>
  
  View()
  
  
  
  

