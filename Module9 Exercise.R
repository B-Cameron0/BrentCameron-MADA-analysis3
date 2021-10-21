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
#Note that our main categorical predictor is nausea
mydata_rec <-
  recipe(Nausea ~ ., data = train_data) 

#Let's get the summary of our data to examine it more closely
summary(mydata_rec)

#Now that we have our recipe for our data, we will fit our model to the data
glm_mod <- logistic_reg() %>%
  set_engine("glm")

#Now that we have set our model, we will use the 'workflow' package to 
#streamline the fitting process by bundling the recipe and models together
#Note that we use our recipe in the process and not the data by itself
mydata_wflow <-
  workflow() %>%
  add_model(glm_mod) %>%
  add_recipe(mydata_rec)

#Now we will prepare the recipe and train the model
mydata_fit <-
  mydata_wflow %>%
  fit(data = train_data)

#Now we will examine our fitted model by using the 'extract_fit' function
mydata_fit %>%
  extract_fit_parsnip() %>%
  tidy()

#With the past commands, so far we have 1. built our model, 2. created a recipe,
#3. bundled the model with our recipe, and 4. trained our workflow using a 
#single call to fit()

#Now we will use our trained workflow to predict with the test data using the 
#predict() command, which applies the recipe to the new data then passes the
#collected information to the fitted model
predict(mydata_fit, test_data)

#We weill use the 'augment' function to predict each possibility of our data
mydata_aug <- augment(mydata_fit, test_data)

mydata_aug %>%
  select(Nausea, .pred_class, .pred_Yes)

#We will now generate a ROC curve to examine the effectiveness of our model
mydata_aug %>%
  roc_curve(truth = Nausea, .pred_Yes ) %>%
  autoplot()

#Now we will use the roc_auc() function to estimate the area under the curve
mydata_aug %>%
  roc_auc(truth = Nausea, .pred_Yes)

#Now that we have examined the ROC and ROC-AUC for the test_data we will examine 
#the ROC and ROC-AUC for the training data
predict(mydata_fit, train_data)
mydata_aug <- augment(mydata_fit, train_data)
mydata_aug %>%
  select(Nausea, .pred_class, .pred_Yes)

#We will now generate a ROC curve to examine the effectiveness of our model
mydata_aug %>%
  roc_curve(truth = Nausea, .pred_Yes ) %>%
  autoplot()

#Now we will use the roc_auc() function to estimate the area under the curve
mydata_aug %>%
  roc_auc(truth = Nausea, .pred_Yes)

#Finally, we will examine an alternative model with only the main predictor 
#RunnyNose, to Nausea, rather than the other chosen predictors
#We must create a new recipe before we continue
alternative_rec <-
  recipe(Nausea ~ RunnyNose, data = train_data) 

#Just like before, we will create a workflow with the recipe
alternative_rec_workflow <-
  workflow() %>%
  add_model(glm_mod) %>%
  add_recipe(alternative_rec)

#Now we will prepare the recipe and train the model
alternative_fit <-
  alternative_rec_workflow %>%
  fit(data = train_data)

#Now we will examine our fitted model by using the 'extract_fit' function
alternative_fit %>%
  extract_fit_parsnip() %>%
  tidy()