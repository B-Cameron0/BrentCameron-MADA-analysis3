---
title: "Module 11 Exercise"
author: "Brent Cameron"
date: "11/5/2021"
output: html_document
---

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
