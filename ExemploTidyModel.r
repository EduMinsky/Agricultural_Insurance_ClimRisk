library(tidymodels)      # for the recipes package, along with the rest of tidymodels
library(ranger)

data1 <- read_rds('./FinalData/Final_Data_readyToUse.rds')%>%as_tibble%>%select(-ID) %>% mutate(Target =factor(Target, levels = c("0","1")) )

set.seed(123)
splits <-initial_split(data1%>%select(-c(x,y)),strata = Target)

cell_train <- training(splits)
cell_test  <- testing(splits)
rf_mod <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")
  set.seed(234)
rf_fit <- 
  rf_mod %>% 
  fit(Target ~ ., data = cell_train)

rf_training_pred <- 
  predict(rf_fit, cell_train) %>% 
  bind_cols(predict(rf_fit, cell_train, type = "prob")) %>% 
  # Add the true outcome data back in
  bind_cols(cell_train %>% 
              select(Target))

rf_testing_pred <- 
  predict(rf_fit, cell_test) %>% 
  bind_cols(predict(rf_fit, cell_test, type = "prob")) %>% 
  bind_cols(cell_test %>% select(Target))

rf_testing_pred %>%                   # test set predictions
  roc_auc(truth = Target, .pred_0 )
  
rf_testing_pred %>%                   # test set predictions
  accuracy(truth = Target, .pred_class)

set.seed(345)
folds <- vfold_cv(cell_train, v = 10)
folds
rf_wf <- 
  workflow() %>%
  add_model(rf_mod) %>%
  add_formula(Target ~ .)

set.seed(456)
rf_fit_rs <- 
  rf_wf %>% 
  fit_resamples(folds)
collect_metrics(rf_fit_rs)
