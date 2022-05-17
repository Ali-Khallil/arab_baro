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
  select(q732B,q730B,q734A,q734B,q104,Q104a_1)|>
  mutate(q734B=case_when(q734B== 1 ~ 'Strongly favor',
                         q734B== 2 ~ 'Favor',
                         q734B== 3 ~ 'Oppose',
                         q734B== 4 ~ 'Strongly oppose',
                         q734B== 98 ~ 'Don’t know',
                         q734B== 99 ~ 'Refused to answer'))|>
  rename(q734b, 'relations between Morocco and Israel'=q734b)|>
  
  
  mutate(q104=case_when(q104== 1 ~ 'yes',
                        q104== 2 ~ 'no',
                        q104== 98 ~ 'Don’t know',
                        q104== 99 ~ 'Refused to answer'))|>
  rename(q104, 'thought about emigrating from your country'=q104)|>
  
  
  mutate(Q104a_1=case_when(Q104a_1== 1 ~ 'Strongly favor',
                           Q104a_1== 2 ~ 'Favor',
                           Q104a_1== 3 ~ 'Oppose',
                           Q104a_1== 4 ~ 'Strongly oppose',
                           Q104a_1== 98 ~ 'Don’t know',
                           Q104a_1== 99 ~ 'Refused to answer'))|>
  rename(Q104a_1, 'Why have you thought about emigrating?'=Q104a_1)|>
  
  View()
  
  
  
  

