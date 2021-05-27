# Load package(s) ----
library(tidyverse)
library(tidymodels)


# load required objects ---- 
load("data/saved_stuff.rda")
load("model_info/knn_tune.rda")
load("model_info/en_tune.rda")
load("model_info/mars_tune.rda")
load("model_info/bt_tune.rda")


# KNN Model ---------------------------------------------------------------

knn_best <- knn_tune %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>% 
  arrange(desc(mean))



# Elastic Net Model  ------------------------------------------------------

en_best <- en_tune %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>% 
  arrange(desc(mean))


en_predicts <- function(i){ 
  

  en_model <- logistic_reg(
    penalty = en_best[[i, 1]],
    mixture = en_best[[i, 2]]
  ) %>% 
    set_mode("classification") %>% 
    set_engine("glmnet")
  
  
  en_workflow <- workflow() %>% 
    add_model(en_model) %>% 
    add_recipe(loan_recipe) 
  
  en_workflow %>% 
    fit(data = training_data) %>% 
    predict(new_data = testing_data) %>% 
    cbind(id = testing_data$id) %>% 
    select("ID" = id, "Category" = .pred_class) 
  
}

for(i in 1:7){
  write.csv(
    en_predicts(i),
    paste0("predictions/en_prediction", i, ".csv"),
    row.names = F
  )
}


# Mars Model  ------------------------------------------------------

mars_best <- mars_tune %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>% 
  arrange(desc(mean))


mars_predicts <- function(i){ 
  
  
  mars_model <- mars(
    num_terms = mars_best[[i, 1]],
    prod_degree = mars_best[[i, 2]]
  ) %>% 
    set_mode("classification") %>% 
    set_engine("earth")
  
  
  mars_workflow <- workflow() %>% 
    add_model(mars_model) %>% 
    add_recipe(loan_recipe) 
  
  mars_workflow %>% 
    fit(data = training_data) %>% 
    predict(new_data = testing_data) %>% 
    cbind(id = testing_data$id) %>% 
    select("ID" = id, "Category" = .pred_class) 
  
}

for(i in 1:7){
  write.csv(
    mars_predicts(i),
    paste0("predictions/mars_prediction", i, ".csv"),
    row.names = F
  )
}




# Boosted Trees Model  ------------------------------------------------------

bt_best <- bt_tune %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>% 
  arrange(desc(mean))


bt_predicts <- function(i){ 
  
  
  bt_model <- boost_tree(
    mtry = bt_best[[i, 1]], 
    min_n = bt_best[[i, 2]],
    learn_rate = bt_best[[i, 3]]
  ) %>% 
    set_mode("classification") %>% 
    set_engine("xgboost")
  
  
  bt_workflow <- workflow() %>% 
    add_model(bt_model) %>% 
    add_recipe(loan_recipe) 
  
  bt_workflow %>% 
    fit(data = training_data) %>% 
    predict(new_data = testing_data) %>% 
    cbind(id = testing_data$id) %>% 
    select("ID" = id, "Category" = .pred_class) 
  
}

for(i in 1:7){
  write.csv(
    bt_predicts(i),
    paste0("predictions/bt_prediction", i, ".csv"),
    row.names = F
  )
}


