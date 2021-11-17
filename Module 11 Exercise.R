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
library(vip)
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

####################################
#Null Model Performance
####################################

#We need to specify our model before we start computing
lm_model <- linear_reg() %>% 
             set_engine('lm') %>%
             set_mode('regression')

#We will now compute the performance of a null model for our training and test data
#(doesn't use any predictor information)

#Train Data Computing
train_null_recipe <- lm(BodyTemp ~ 1, data = train_data)

#Calculating RMSE
train_null_recipe %>% augment(newdata = train_data) %>%
                      rmse(truth = BodyTemp, estimate = .fitted)

#Test Data Computing
test_null_recipe <- lm(BodyTemp ~ 1, data = test_data)

#Calculating RMSE
test_null_recipe %>% augment(newdata = test_data) %>%
  rmse(truth = BodyTemp, estimate = .fitted)

#################################
#Model Tuning and Fitting
#################################

#We will fit a tree, LASSO model, and a random forest 
#Our steps should be as follows...
#1. Model Specification
#2. Workflow Definition
#3. Tuning Grid Specification
#4. Tuning Using Cross- Validation and the tune_grid() function

#Code Used for Tree model can be found from Tidymodels Tutorial 
#https://www.tidymodels.org/start/tuning/

####################################
#TREE
####################################

#Specify Model
tune_spec_TREE <-
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune(),
  ) %>%
  set_engine("rpart") %>%
  set_mode("regression")
tune_spec_TREE
    
#We will now define the workflow for the tree 
workflow_TREE <- workflow() %>%
            add_model(tune_spec_TREE) %>%
            add_recipe(recipe_bodytemp) #The recipe command used here is from 
#line 96 where we discuss creating a recipe for data and fitting

#We will now specify the tuning grid
grid_TREE <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 5)
grid_TREE

#We will now tune using cross validation and the tune_grid() function
res_TREE<-
  workflow_TREE %>%
  tune_grid(resamples = FoldCV5 , grid = grid_TREE, metrics = metric_set(rmse))

#Now we will run the autoplot() function to look at some diagnostics
res_TREE %>%
  autoplot()

#Now we will selecect the best decision tree model
TOP_TREE <- res_TREE %>% 
  select_best("rmse")

TOP_TREE

#Now we need to finalize the workflow
workflow_FINAL <- workflow_TREE %>% finalize_workflow(TOP_TREE)
workflow_FINAL

#Now we will utilize the fit() function to fit to the training data
fit_FINAL_TREE <- workflow_FINAL %>% last_fit(data_split)

#Now we will collect the data from our fit
fit_FINAL_TREE %>% collect_metrics()

#We will also collect the predictions
pred_TREE <- fit_FINAL_TREE %>% collect_predictions()

#We will now make two plots, one that shows model predictions from the tuned
#model compared to actual outcomes, and one that plots residuals (RMSE)
pred_tree_plot <- ggplot(data = pred_TREE, aes(x = .pred, y = BodyTemp)) + 
           geom_point() +
           labs(title = "Plot Comparing Model Predictions from Tuned to Actual",
                x = "Predictions", y = "Outcomes")
#view the plot
pred_tree_plot

#We need to calculate our residuals before we can plot 
#Note that the residuals is the difference between our main predictor and the others
pred_TREE$residuals <- pred_TREE$BodyTemp - pred_TREE$.pred

#Now we will plot our residuals
resid_tree_plot <- ggplot(data= pred_TREE, aes(x=.pred , y=residuals)) + geom_point() +
                    labs(title="Plot of Residuals",
                    x="Predictions", y= "Residuals")
#view the plot
resid_tree_plot 

#Now we will compare our residual plot to the null model
tree_model_performance <- res_TREE %>% show_best(n=1)
print(tree_model_performance)

#The null tree and decision tree model perform very similarly, with a
#null RMSE of 1.14 and a decision tree RMSE of 1.19, the decision tree model 
#does not perform better than the null

#########################################
#LASSO
#########################################
#Now we will construct a LASSO model
#code used from https://www.tidymodels.org/start/case-study/

#We will once again start by constructing our model
lasso_model <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("glmnet") %>%
  set_args(penalty = tune(), mixture = 1) 

#Please note that mixture refers to a number between zero and one that is the 
#proportion of L1 regularization (lasso) in the model. In other, words, because
#we are using mixture = 1, we are utilizing a "pure" lasso model here

#We will now create our workflow
lasso_workflow <-workflow() %>%
  add_model(lasso_model) %>%
  add_recipe(recipe_bodytemp)

#Now we will tune our LASSO model
#As our last model took a long time to run, we will utilize parallel computing
#to make it faster

library(doParallel)

ncores = 5 #Ncores is used to select the number of cores you want to recruit
#for processing, different computers will naturally have different ideal numbers

cluster <- makePSOCKcluster(5) #make PSOCKcluster stands for creating a sock
#cluster within the 'snow' package, this allowsa for increased computing time

  registerDoParallel(5) #registers parallel backend with foreach package
  
  #Now we will create our tuning grid 
  lasso_reg_grid <- tibble(penalty = 10^seq(-3, 0, length.out = 30))
  #Now we tune the model
  lasso_tune_res <- lasso_workflow %>%
    tune_grid(resamples = FoldCV5,
              grid = lasso_reg_grid,
              control = control_grid(save_pred = TRUE),
              metrics = metric_set(rmse))
  
#We will now turn off parallel clustering, the reason we turn the clustering off
#after each use is to prevent computations and analysis from being slowed in 
#later data analysis, fitting, modeling, etc.
  
  #We will now evaluate our LASSO model
  lasso_tune_res %>% autoplot()
  
  #Now we will get the tuned model that performs best
  best_lasso <- lasso_tune_res %>% select_best(metric = "rmse")
  #We now finalize our workflow with the best model
  best_lasso_wf <- lasso_workflow  %>% finalize_workflow(best_lasso)
  #We now fit our best performing model
  best_lasso_fit <- best_lasso_wf %>%
    fit(data = train_data)
  
  lasso_pred <- predict(best_lasso_fit, train_data)
  
  #Now we will repeat our steps like the past model and plot LASSO variables as
  #function of tuning parameter
  
x <- best_lasso_fit$fit$fit$fit
plot(x, "lambda")

#When a variable is 0 it is no longer being used in the model, thus we are using
#all variables that are only part of the best fit model
tidy(extract_fit_parsnip(best_lasso_fit)) %>% filter(estimate !=0)

#Now we plot the observed/predicted and residual plots
#We will try a new way to plot that does not require calculating the 
#residuals before hand

#First we will plot with the observe/predicted values
#This code will plot a line with which we hope to see overlap with 
#the values, thus signaling that the model is a good fit

#For our x and y limits, the values 97 and 103 were chosen because they allow for
#the clearest illustration of the values in the plane
#The abline is used to add lines to the graph 
plot(lasso_pred$.pred,train_data$BodyTemp, xlim = c(97, 103), ylim = c(97, 103))
abline(a = 0, b = 1, col = 'red') #b = 1 creates a 45 degree diagonal line

#Now our residual plot, note that because we are subtracting the two values 
#used this time instead of putting them together, since residuals are by 
#definition the difference between the regular predictors and the chosen predictor
plot(lasso_pred$.pred-train_data$BodyTemp)
abline(a=0, b=0, col = 'blue') #b = 0 creates a straight horizontal line

#Similarly to the tree model, neither the observed/predictors plot or the 
#residuals plot indicates that there is significant alignment with the data
#meaning that this model is also not significant.

#Let's look at the performance of the model directly to check this
lasso_performance <- lasso_tune_res %>%
                        show_best(n = 1)

print(lasso_performance)

#The mean RMSE is 1.17, which is still not very impressive, a much lower value
#would be preferred

################################
#RANDOMFOREST
################################

#Both of our past models have not shown significant fit, we will now repeat the
#steps with a random forest model in the hopes of finding significance

###Please note that for Random Forest models, "num.threads" and importance 
#is required or else all models will fail
randomforest_model <- rand_forest() %>%
  set_args(mtry = tune(),
  trees = tune(), 
  min_n = tune()
  ) %>%
  #Now we set the engine
  set_engine("ranger", 
             num.threads = 5,
             importance = "permutation") %>%
  #We select either the continous or binary classification
  set_mode("regression")

#We will set our workflow once again
randomforest_workflow <- workflow() %>% 
  add_model(randomforest_model) %>%
  add_recipe(recipe_bodytemp)

#We will now repeat our steps as the first two models to specify our tuning grid
#We will use parallel computing once again to vastly decrease the time it takes 
#to compute the model- since we have already use code previously to create it
#we now only need to use our name designation for our cluster and it will resume
cluster <-makePSOCKcluster(5)
registerDoParallel(5)

#Now we will tune the grid
randomforest_grid <- expand.grid(mtry = c(3, 4, 5, 6), min_n = c(40, 50, 60),
                                 trees = c(500, 1000))

#We will now tune the model while optimizing RMSE
randomforest_tune_res <- randomforest_workflow %>%
    tune_grid(resamples = FoldCV5, #This is the name of our previous CV object
              grid = randomforest_grid,#This is the grid of values we want to try
              metrics = metric_set(rmse))

#Now we turn off our parallel clustering again to prevent slowing processing
stopCluster(cluster)

#Now we plot the performance of our different tuning parameters
randomforest_tune_res %>% autoplot()

#Now we will obtain the best performing model
best_randomforest <- randomforest_tune_res %>% select_best(metric = "rmse")

#Finalize the workflow with this model
best_randomforest_workflow <- randomforest_workflow %>% finalize_workflow(best_randomforest)

#Now we fit the best performing model
best_randomforest_fit <- best_randomforest_workflow %>% fit(data = train_data)
randomforest_predict <-predict(best_randomforest_fit, train_data)

#although all variables stay in a random forest model, we can examine which are 
#the most imoportant using the 'vip' package
x<- best_randomforest_fit$fit$fit$fit

#plot the variables by importance
vip(x, num_features = 26)

#as can be seen from the plot, subjective fever is the most important variable
#followed by sneezing 

#We will now plot the observed/ predicted and residual plots and compare them
#we will repeat the same process used as last time
plot(randomforest_predict$.pred,train_data$BodyTemp, 
     xlim =c(97, 103), ylim=c(97, 103),
     abline(a = 0, b = 1, col = 'red'))

#residual plot
plot(randomforest_predict$.pred-train_data$BodyTemp)
     abline(a = 0, b = 0, col = 'blue')
     
#now that we have finished plotting lets look at our model performance
randomforest_performance <- randomforest_tune_res %>% show_best(n = 1)
print(randomforest_performance)

#The mean RMSE is 1.18, which is still not significant

#The LASSO model had the lowest RMSE, which even though is not significant 
#compared to the null model, still puts in in a position to be chosen as the 
#most meaningful

########################################
#final model (LASSO) fitting
########################################
#lets restart our parallel processing
cluster<- makePSOCKcluster(5)
registerDoParallel(5)

#Now we will fit on the training set evaluating with the test data
LASSO_fit_final <-best_lasso_wf %>% last_fit(data_split)

#We will now use a trained workflow to predict using our 
#test data
final_test_performance<-LASSO_fit_final %>% collect_predictions()
print(final_test_performance)

final_test_performance_RMSE <- LASSO_fit_final %>% collect_metrics()
print(final_test_performance_RMSE)

#When comparing the prediction of our final model with the actual data, it 
#appears to be rather close, which indicates that we thankfully avoided 
#overfitting the data

#unfortunately, when we examine the RMSE of our data we can see that it performs
#exactly the same as with the last data. While this shows the model is consistent
#it still indicates that the model is not an adequate fit for our data

#We will finally plot our final models predicted compared with observed values
#and another plot for residuals

#predicted versus observed 
plot(final_test_performance$.pred, test_data$BodyTemp, 
     xlim = c (97, 103), ylim = c(97, 103))
     abline(a = 0, b = 1, col = 'red')
     
#residual plot
plot(final_test_performance$.pred-test_data$BodyTemp)
     abline(a = 0, b = 0, col = 'red')
     


     

