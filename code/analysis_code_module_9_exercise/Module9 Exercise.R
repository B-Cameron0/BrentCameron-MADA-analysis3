###########################
#Module 9 Exercise
###########################

#load needed packages. make sure they are installed.
library(tidyverse) #for streamlining manipulating data
library(tidymodels) # for streamlining fitting data to models
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving

#Now we determine the location of the data
data_location <- here::here("data","processed_data","exploration.rds")

#load data. 
mydata <- readRDS(data_location)

#Now that we have processed, explored, and analyzed the data, we will now 
#work on splitting the data to create a training set and a testing set, 
#which we will then use to measure model performance

#First we fix the random numbers by setting the seed
#This allows for the construction of a reproducible analysis when random numbers
#are utilized
set.seed(123)

#We will now put 3/4 of our data into the training set
data_split <-initial_split(mydata, prop = 3/4)

#We will now create data frames for the two sets (training and testing)
train_data <- training(data_split)

test_data <- testing(data_split)

#Now that we have data that is split into our training and test data, we will
#now initiate a recipe to create a simple linear regression model
mydata_rec <-
  recipe(RunnyNose ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors())

#Let's get the summary of our data to examine it more closely
summary(mydata_rec)


