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
  select(q700b_1,q700b_13,q700b_23,q700b_5,
         q725_1,q725_4,q725_15,q725_23,q727_1,q727_4,q727_5,
         q732b,q730b,q734a,q734b,q104,q104a_1)|>
  
  mutate(q700b_1=case_when(q700b_1== 1 ~ 'Very favorable',
                             q700b_1== 2 ~ 'Somewhat favorable',
                             q700b_1== 3 ~ 'Somewhat unfavorable',
                             q700b_1== 4 ~ 'Very unfavorable',
                             q700b_1== 98 ~ 'Don’t know',
                             q700b_1== 99 ~ 'Refused to answer',
                                        T ~ 'na'))|>
  rename(q700b_1,'The United States'=q700b_1)|>
  View()
  
  mutate(q700b_13=case_when(q700b_13== 1 ~ 'Very favorable',
                           q700b_13== 2 ~ 'Somewhat favorable',
                           q700b_13== 3 ~ 'Somewhat unfavorable',
                           q700b_13== 4 ~ 'Very unfavorable',
                           q700b_13== 98 ~ 'Don’t know',
                           q700b_13== 99 ~ 'Refused to answer',
                           T ~ 'na'))|>
  rename(q700b_13, 'China'=q700b_13)|>
  
  mutate(q700b_23=case_when(q700b_23== 1 ~ 'Very favorable',
                            q700b_23== 2 ~ 'Somewhat favorable',
                            q700b_23== 3 ~ 'Somewhat unfavorable',
                            q700b_23== 4 ~ 'Very unfavorable',
                            q700b_23== 98 ~ 'Don’t know',
                            q700b_23== 99 ~ 'Refused to answer',
                            T ~ 'na'))|>
  rename(q700b_23, 'Germany'=q700b_23)|>
  
  mutate(q700b_5=case_when(q700b_5== 1 ~ 'Very favorable',
                            q700b_5== 2 ~ 'Somewhat favorable',
                            q700b_5== 3 ~ 'Somewhat unfavorable',
                            q700b_5== 4 ~ 'Very unfavorable',
                            q700b_5== 98 ~ 'Don’t know',
                            q700b_5== 99 ~ 'Refused to answer',
                            T ~ 'na'))|>
  rename(q700b_5, 'France'=q700b_5)|>
    
    
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
  
  
  mutate(q104a_1=case_when(q104a_1== 1 ~ 'For economic reasons',
                           q104a_1== 2 ~ 'For political reasons ',
                           q104a_1== 3 ~ 'Religious reasons',
                           q104a_1== 4 ~ 'Security reasons',
                           q104a_1== 5 ~ 'Education opportunities for self or family members',
                           q104a_1== 6 ~ 'Reunite with family',
                           q104a_1== 7 ~ 'Corruption',
                           q104a_1== 8 ~ 'For other reasons',
                           q104a_1== 98 ~ 'Don’t know',
                           q104a_1== 99 ~ 'Refused to answer',
                           T ~ 'na'))|>
  rename(q104a_1, 'Why have you thought about emigrating?'=q104a_1)|>
  
  View()
  
  
  #--------------------------------------------
  
  arab_data_demo <- arab_data |>
    rename_with(.fn = tolower,.col=everything())|>
    filter(country==7)|> 
    select(q1003,q1010,q1010b1,q1010b2,q1014c,q1005,q1005b
           q1005c,q1005d,q1005e,q1006a,q1006b_code)|>
    mutate(q=case_when(q734b== 1 ~ 'Strongly favor',
                       q734b== 2 ~ 'Favor',
                       q734b== 3 ~ 'Oppose',
                       q734b== 4 ~ 'Strongly oppose',
                       q734b== 98 ~ 'Don’t know',
                       q734b== 99 ~ 'Refused to answer',
                       T ~ 'na'))|>
    rename(q734b, 'relations between Morocco and Israel'=q734b)|>
    
    #q700b_1,q700_13,q700_23,q700_5,q732b,q730b, q725

