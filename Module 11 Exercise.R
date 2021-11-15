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
################################
#Part 2- Analysis
################################

