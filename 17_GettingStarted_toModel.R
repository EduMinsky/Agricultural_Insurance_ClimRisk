library(tidyverse)
library(sf)
library(tidymodels)
library(parsnip)
library(mda)
library(ggplot2)
library(brulee)
library(glmnet)

data_train <-read_rds('./FinalData/Final_Data_readyToUse.rds')%>%tibble
data_train$Target <- factor(data_train$Target)
data_train$Target%>%table

#spat_train <- st_as_sf (data_train,coords=c('x',"y"))
#spat_train<- spat_train %>% st_difference()
#a <- spat_train%>%st_drop_geometry 

data_train_model <- data_train %>%select(-c("ID","x","y"))
splits <- initial_split(data_train_model, strata =Target )
data_training <- training(splits)
data_testing <- testing(splits)
logreg <- logistic_reg(mode = "classification",penalty = tune(), engine = "glmnet")

receita <- recipe(Target ~.,data = data_training) #%>%
      #step_normalize(all_numeric_predictors())


wf <- workflow() %>%
  add_model(logreg) %>% add_recipe(receita)

v_fold <- vfold_cv(data_training,v = 10, strata = Target)
trained_lda <- wf %>% tune_grid(v_fold, grid = 10, control = control_grid(), metrics = metric_set(accuracy))

trained_lda%>%show_best(n=5)
ggplot2::autoplot(trained_lda)

