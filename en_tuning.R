# Elastic Net tuning ----

# Load package(s) ----
library(tidyverse)
library(tidymodels)

# load required objects ----
load("data/saved_stuff.rda")

# Define model ----
en_model <- logistic_reg(
  penalty = tune(),
  mixture = tune()
) %>% 
  set_mode("classification") %>% 
  set_engine("glmnet")


# set-up tuning grid ----
# update parameters 
en_params <- parameters(en_model) 

# define tuning grid
en_grid <- grid_regular(en_params, levels = 10) 

# workflow ----
en_workflow <- workflow() %>% 
  add_model(en_model) %>% 
  add_recipe(loan_recipe) 

# Tuning/fitting ----
en_tune <- en_workflow %>% 
  tune_grid(
    resample = loan_folds, 
    grid = en_grid
  ) 


# Write out results & workflow
save(en_tune, en_workflow, file = "model_info/en_tune.rda")
