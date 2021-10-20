###############################
# Data Analysis
###############################

#Before we engage in data modeling we must load the necessary packages and load
#the data

#load needed packages. make sure they are installed.
library(tidyverse) #for streamlining manipulating data
library(tidymodels) # for streamlining fitting data to models
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving

#Now we determine the location of the data
data_location <- here::here("data","processed_data","exploration.rds")

#load data. 
mydata <- readRDS(data_location)

#Before we proceed we must remember our previously chosen important variables
#(from the exploration.rds) along with a newly chosen main predictor of interest
#which is runnynose

#Variables of interest are chosen to help with identifying the correlation between
#body temp and nausea

#Variables chosen include- Fatigue, Headache, Weakness, Breathless, and 
#SubjectiveFever, as well as runnynose

######################################
#Model Fitting
######################################

#First we must set the engine we will use for the modeling
#We will be doing simple linear regression modeling 
#since we are using the linear regression model we will name the model lrm_mod
lrm_mod <- linear_reg() %>%
  set_engine("lm")

#First we will fit a linear model to the continuous outcome (bodytemp) using
#only the main predictor of interest (RunnyNose)
# fit linear model
lrm_fit1 <- lrm_mod %>%
  fit(BodyTemp ~ RunnyNose, data = mydata)

#Now we utilize the tidyverse package to organize the data collected into a 
#tibble
tidy(lrm_fit1)

#RunnyNose has a p-value of <.05 (0.00268), signaling possible statistical
#significance between RunnyNose and body temperature

#Now we will fit another linear model to the continuous outcome (again, Bodytemp)
#using all chosen predictors of interest (including runnynose)

#Remember that variables chosen include- Fatigue, Headache, Weakness, Breathless, 
#SubjectiveFever, in addition to our main predictor of interest, runnynose

lrm_fit2 <- lrm_mod %>%
  fit(BodyTemp ~ RunnyNose + Weakness + Fatigue + Headache + Breathless 
      + SubjectiveFever, data = mydata)

#Now we will repeat placing the data from the model into a tibble for easier
#organization and encapsulation
tidy(lrm_fit2)

#From the model it appears that fatigue is a moderate predictor of bodytemp 
#(0.0664), runnynose is a strong predictor (0.00303) and subjective fever
#is an incredibly strong predictor with a p-value of (0.000000329).

#Now we can begin with comparing the models, we will use ANOVA 
#(analysis of variance) for this purpose, we use the tidy() function for 
#data organization
anova <- anova(lrm_fit1$fit , lrm_fit2$fit) %>% 
  tidy()

#We retype the name of our given anova model to present the data gathered
anova

#Since the p-value is less than 0.05 (2.99e^-07), we can see that there is 
#a difference between the two models (model 1 with no interaction between
#variables, and model 2, which has interaction between variables). This means 
#that the chosen predictor variables have an affect on the data

#For our next model, since we will be using discrete data, we need to
#change our model engine from a linear regression model to a generalized
#logistics model

glm_mod <- logistic_reg() %>%
  set_engine("glm")

#now that the new engine is created we can fit our variables as before

#To fit a logistic model to the categorical outcome (nausea)
#using only the main predictor of interest we will use the following commands
#We will reuse the variable RunnyNose, since it is our main predictor of 
#interest
glm_fit1 <- glm_mod %>%
  fit(Nausea ~ RunnyNose, data = mydata) %>%
  tidy()

glm_fit1

#The intercept seems to be significant with a p-value of <0.05 (0.00000589),
#while RunnyNose is not (>0.05, 0.770)

#We will now fit another logistical model to the categorical outcome (nausea)
#that uses all predictors of interest
glm_fit2 <- glm_mod %>%
  fit(Nausea ~ RunnyNose + Weakness + Fatigue + Headache + Breathless 
      + SubjectiveFever, data = mydata) %>%
  tidy()

glm_fit2

#We can see that there exists possible statistical significance between the 
#variables of weaknesssevere(0.006333), breathlessyes(0.000528), and the 
#intercept(0.00000433)

#Finally, we will compare the categorical models
#The first model with RunnyNoseYes seems to show that RunnyNose is not a
#significant predictor with a p-value of >0.05 and an intercept p-value of <0.05
#The second model indicates that WeaknessSevere and BreathlessYes are both
#statistically significant predictors with p-values less than 0.05.



