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
processeddata <- readRDS(data_location)

#take a look at the data
dplyr::glimpse(processeddata)

#our main continuous outcome of interest is body temperature, and our main
#categorical outcome of interest is nausea. We will determine if other symptoms
#are correlated with both

#We will start with body temperature, which is our main continuous predictor
#first we will look at a brief summary
summary(processeddata$BodyTemp)

#The median of the data is 98.50
#Next we will create a basic histogram plot of the BodyTemp variable

bodytemp_hist_plot <-ggplot(processeddata,aes(BodyTemp)) + 
                geom_histogram()

#Now we will print the plot and save it
print(bodytemp_hist_plot)
saveRDS(bodytemp_hist_plot, file = here("results", "bodytemp_hist_plot.rds"))

#The histogram of body temperature shows that the average temperature is 
#around 98, with several outliers from 100-102 and beyond. 

#We will no look at a summary of our main categorical predictor, nausea
summary(processeddata$Nausea)

#there are 255 reported 'yes' and 475 reported 'no'

#Now we will plot our main categorical outcome nausea

naus_plot <- ggplot(processeddata, aes(Nausea)) +
             geom_bar()

print(naus_plot)

#The bar gives a visual representation of reported cases of nausea

#Save the plot
saveRDS(naus_plot, file = here("results", "naus_plot.rds"))

#Now that we have done a basic exploration of our main variables of interest
#we will now examine several chosen predictor variables that we are interested
#in

#Variables of interest are chosen to help with identifying the correlation between
#body temp and nausea

#Variables chosen include- Fatigue, Headache, Weakness, Breathless, and 
#SubjectiveFever

#We will start by comparing our main variables of interest (body temp and nausea)
#note that since bodytemp is continous, it will be on the y axis
ggplot(processeddata, aes(Nausea, BodyTemp)) +
  geom_boxplot()

#Now we will look at body temp and fatigue
ggplot(processeddata, aes(Fatigue, BodyTemp)) +
  geom_boxplot()

#body temp and Headache
ggplot(processeddata, aes(Headache, BodyTemp)) +
  geom_boxplot()

#body temp and Weakness
ggplot(processeddata, aes(Weakness, BodyTemp)) +
  geom_boxplot()

#body temp and Breathless
ggplot(processeddata, aes(Breathless, BodyTemp)) +
  geom_boxplot()

#body temp and SubjectiveFever
ggplot(processeddata, aes(SubjectiveFever, BodyTemp)) +
  geom_boxplot()

##################
#Now we will examine the variables of interest with nausea as the main categorical
#variable
#note that since we are now using our main categorical variable nausea, it will
#be used to fill the x axis

#Now we will look at Nausea and fatigue
ggplot(processeddata, aes(Nausea, Fatigue)) +
  geom_bin_2d()

#Nausea and Headache
ggplot(processeddata, aes(Nausea, Headache)) +
  geom_bin_2d()

#Nausea and Weakness
ggplot(processeddata, aes(Nausea, Weakness)) +
  geom_bin_2d()

#Nausea and Breathless
ggplot(processeddata, aes(Nausea, Breathless)) +
  geom_bin_2d()

#Nausea and SubjectiveFever
ggplot(processeddata, aes(Nausea, SubjectiveFever)) +
  geom_bin_2d()
                 
#Weakness and SubjectiveFever seem to be the best possible 
#predictors of body temp
          
#Headache and Fatigue seem to be the best possible 
#predictors of nausea


# location to save file
save_data_location <- here::here("data","processed_data","exploration.rds")

saveRDS(processeddata, file = save_data_location)