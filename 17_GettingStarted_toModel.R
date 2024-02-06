library(tidyverse)
data_train <-read_rds('./FinalData/Final_Train_Data_readyToUse.rds')%>%tibble
data_train%>%glimpse
data_train$Target <- factor(data_train$Target)
data_train%>%glimpse
data_train

library(tidymodels)
a <- discrim_linear(
  mode = "classification",
  penalty = NULL,
  regularization_method = NULL,
  engine = "MASS"
)
a
