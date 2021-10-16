###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#load needed packages. make sure they are installed.
library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","raw_data","SympAct_Any_Pos.Rda")

#load data. 
rawdata <- readRDS(data_location)

#take a look at the data
dplyr::glimpse(rawdata)

#Remove all of the following variables: Score, Total, FluA, FluB, Dxname, 
#Activity, Unique.Visit
processeddata = rawdata[ , !(names(rawdata) %in% c("DxName1",
           "DxName2", "DxName3", "DxName4", "DxName5", "Unique.Visit",
           "ActivityLevel","ActivityLevelF","RapidFluA", "RapidFluB","PCRFluA",
           "PCRFluB","TransScore1","TransScore1F","TransScore2","TransScore2F",
           "TransScore3","TransScore3F","TransScore4","TransScore4F",
           "ImpactScore","ImpactScore2","ImpactScore3","ImpactScoreF",
           "ImpactScore2F","ImpactScore3F","ImpactScoreFD","TotalSymp1",
           "TotalSymp1","TotalSymp1F","TotalSymp2","TotalSymp3"))]

#Check to see if any presence of NA observations
na.fail(processeddata)

#There are, now we check the data to find NA observations
is.na(processeddata)

#Omit all NA observations for ease of data utilization
 processeddata <-na.omit(processeddata)
 
 processeddata

# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")

saveRDS(processeddata, file = save_data_location)


