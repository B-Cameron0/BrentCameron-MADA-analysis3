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

####### Zane's section #######
# First set up the model
linear_mod <- linear_reg() %>%
  set_engine("lm")

### Runny nose as only predictor
# Set up recipe
temp_rn_rec <- recipes::recipe(BodyTemp ~ RunnyNose, data = train_data)

# Set up workflow
temp_run_wf <- workflows::workflow() %>%
  workflows::add_recipe(temp_rn_rec) %>%
  workflows::add_model(linear_mod)

# Fit model
temp_run_fit <- temp_run_wf %>%
  parsnip::fit(data = train_data)

temp_run_train_pred <- temp_run_fit %>%
  augment(new_data = train_data) %>%
  dplyr::mutate(
    data = "train",
    model = "RunnyNose"
  )

temp_run_test_pred <- temp_run_fit %>%
  augment(new_data = test_data) %>%
  dplyr::mutate(
    data = "test",
    model = "RunnyNose"
  )

### Model with all predictors
# Set up recipe
temp_all_rec <- recipes::recipe(BodyTemp ~ ., data = train_data)

# Set up workflow
temp_all_wf <- workflows::workflow() %>%
  workflows::add_recipe(temp_all_rec) %>%
  workflows::add_model(linear_mod)

# Fit model
temp_all_fit <- temp_all_wf %>%
  parsnip::fit(data = train_data)

temp_all_train_pred <- temp_all_fit %>%
  augment(new_data = train_data) %>%
  dplyr::mutate(
    data = "train",
    model = "All predictors"
  )

temp_all_test_pred <- temp_all_fit %>%
  augment(new_data = test_data) %>%
  dplyr::mutate(
    data = "test",
    model = "All predictors"
  )

# Bind predictions together
temp_preds <- dplyr::bind_rows(
  temp_run_test_pred,
  temp_run_train_pred,
  temp_all_test_pred,
  temp_all_train_pred
)

# Calculate metrics for each set of predictions
temp_mets <- temp_preds %>%
  dplyr::group_by(data, model) %>%
  yardstick::metrics(truth = BodyTemp, estimate = .pred)

# Plot metrics
metrics_plot <- temp_mets %>%
  # Clean up names of metrics to be more understandable
  dplyr::mutate(
    .metric = forcats::fct_recode(
      .metric,
      "R squared" = "rsq",
      "Mean absolute error (MAE)" = "mae",
      "Root mean squared error (RMSE)" = "rmse"
    )
  ) %>%
  # Make a bar plot
  ggplot(aes(y = model, x = .estimate, fill = data)) +
  geom_col(position = "dodge") +
  facet_wrap(vars(.metric), scales = "free_x", ncol = 1) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(color = "black"),
    strip.text = element_text(face = "bold")
  ) +
  scale_fill_manual(name = "data set", values = c("#E69F00", "#56B4E9")) +
  labs(y = NULL, x = NULL)
ggsave(here::here("results", "metric_plot.png"), plot = metrics_plot)
metrics_plot
