# Boosted Tree tuning ----

# Load package(s) ----
library(tidyverse)
library(tidymodels)

# load required objects ----
load("data/saved_stuff.rda")


# Define model ----
bt_model <- boost_tree(
  mtry = tune(), 
  min_n = tune(),
  learn_rate = tune()
) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost")


# set-up tuning grid ----

# update parameters 
bt_params <- parameters(bt_model) %>% 
  update(mtry = mtry(range = c(2, 100)))

# define tuning grid
bt_grid <- grid_regular(bt_params, levels = 10) 

# workflow ----
bt_workflow <- workflow() %>% 
  add_model(bt_model) %>% 
  add_recipe(loan_recipe) 

# Tuning/fitting ----
bt_tune <- bt_workflow %>% 
  tune_grid(
    resample = loan_folds, 
    grid = bt_grid
  ) 



# Write out results & workflow
save(bt_tune, bt_workflow, file = "model_info/bt_tune.rda")
