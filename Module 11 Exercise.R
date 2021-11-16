---
title: "Module 11 Exercise"
author: "Brent Cameron"
date: "11/5/2021"
output: html_document
---
  
#################################
#Module 11 Exercise
#################################

#First we need to load all required packages 
library(tidyverse) #for streamlining manipulating data
library(tidymodels) # for streamlining fitting data to models
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(ggplot2) #for plotting
library(rpart) #for fitting tree model
library(glmnet) #for fitting LASSO model
library(ranger) #for fitting random forest model

#First we need to input the location of the data
data_location <- here::here("data","processed_data","exploration.rds")

#load data. 
mydata <- readRDS(data_location)

#Basic examination of our data
glimpse(mydata)

###################################
#Part 1- Pre-Processing
###################################
#We will remove several variables that have yes/no versions while keeping the 
#versions of the variables that have multiple levels to reduce potential 
#confounding
mydata2 <- mydata %>%
  select(!c(WeaknessYN,CoughYN, CoughYN2, MyalgiaYN))

#Mydata2 now shows that the number of variables has been reduced by 4, which
#is what we want, now we will code the 3 symptom severity (ordinal) factors
#as ordered and verify the correct order of none/mild/moderate/severe
mydata3 <- mydata2 %>%
  mutate(Weakness = factor(Weakness, levels = c("None", "Mild", "Moderate","Severe"), ordered = TRUE))%>%
  mutate(CoughIntensity = factor(CoughIntensity, levels = c("None", "Mild", "Moderate","Severe"), ordered = TRUE)) %>%
  mutate(Myalgia = factor(Myalgia, levels = c("None", "Mild", "Moderate","Severe"), ordered = TRUE))

#Now that we have hopefully coded the variables to be ordered we will need to 
#verify that they have been changed
is.ordered(mydata3$Weakness)
is.ordered(mydata3$CoughIntensity)
is.ordered(mydata3$Myalgia)

#Now that we have ordered the variables correctly we will examine our other 
#features. Two of our variables has less than 50 observations and are thus 
#unbalanced, we will need to remove them to aid in accurate data modeling
summary(mydata3)

#Hearing and VIsion both have less than 50 recorded observations in their 
#respective "yes" category so we will remove those two variables
#Note how we create another data set (mydata4) to allow for easier back and forth
#between data sets if needed
mydata4 <- mydata3 %>%
  select(!c(Hearing, Vision))

#Now let us verify that everything is correct and the variables are removed
glimpse(mydata4)

#Everything looks good, all variables that can affect our models have been removed
#We will rename the data to a more accessible name
finaldata <- mydata4

################################
#Part 2- Analysis
################################

#Now that we have pre-processed our data we can begin our analysis, first we 
#will set our seed
set.seed(123)

#We will now split the data by 70% for our training data and 30% for our testing
data_split <- initial_split(finaldata, prop = 7/10,#7/10 stands for 70% training
                            strata = BodyTemp) # and the rest (30%) for testing) 

#Now we will organize our sets of training and test data
train_data <- training(data_split)

test_data <- testing(data_split)

#We will now utilize a 5-fold cross validation, 5 times repeated, we will 
#stratify on "BodyTemp" for the CV folds
FoldCV5 <- vfold_cv(train_data, v = 5, repeats = 5, strata = "BodyTemp")

#Now we will create our recipe for our data and fitting
#We will code the categorical variables as dummy variables
recipe_bodytemp <-recipe(BodyTemp ~ ., data = train_data) %>%
                  step_dummy(all_nominal_predictors())
