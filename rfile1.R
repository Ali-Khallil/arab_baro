install.packages(c('readr','here','dplyr','janitor')) #to install the packages

library(readr) # To read our .csv data
library(here) # To know the project path
library(dplyr) # to manipulate data
library(janitor) #To make all the columns names snake_case

# reading data "imdbTop250" -----------------------------------------------

print(getwd())
arab_data <- read_csv( here("Arab_Barometer.csv") )
arab_data <- clean_names(arab_data) 
