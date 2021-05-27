# Elastic Net tuning ----

# Load package(s) ----
library(tidyverse)
library(tidymodels)

# load required objects ----
load("data/saved_stuff.rda")


# Define model ----
mars_model <- mars(
  num_terms = tune(),
  prod_degree = tune()
) %>% 
  set_mode("classification") %>% 
  set_engine("earth")


# set-up tuning grid ----

# update parameters 
mars_params <- parameters(mars_model) %>% 
  update(num_terms = num_terms(c(2, 100)))

# define tuning grid
mars_grid <- grid_regular(mars_params, levels = 15) 

# workflow ----
mars_workflow <- workflow() %>% 
  add_model(mars_model) %>% 
  add_recipe(loan_recipe) 

# Tuning/fitting ----

mars_tune <- mars_workflow %>% 
  tune_grid(
    resample = loan_folds, 
    grid = mars_grid
  ) 


# Write out results & workflow
save(mars_tune, mars_workflow, file = "model_info/mars_tune.rda")
