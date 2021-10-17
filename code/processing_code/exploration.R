###############################
# exploration script
#
#this script loads the processed data rds file and does a preliminary data
#exploration which is then saved as another Rds file in the processed_data folder

#load needed packages. make sure they are installed.
library(tidyverse) #for data exploration 
library(scales) #for help with axis of data
library(here) #to set paths

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
rawdata <- readRDS(data_location)

#take a look at the data
dplyr::glimpse(rawdata)

#our main continous outcome of interest is body temperature, and our main
#categorical outcome of interest is nausea. We will determine if other symptoms
#are correlated with both







# location to save file
save_data_location <- here::here("data","processed_data","exploration.rds")

saveRDS(processeddata, file = save_data_location)