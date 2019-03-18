#Base setup for neural network modeling
#libraries
library(aws.s3)
library(aws.signature)
library(dplyr)
library(doParallel)
library(tidyr)
library(caret)
library(keras)
library(tibble)
library(tensorflow)
library(foreach)
library(doMC)

#Get model types
source('RScripts/elbtu_nn_model_functions_build.R')
  
#Load input data and assign as train/test/validation
s3load('ModelSaves/elbtu_nn_input.RData', bucket = 'cuny-msds-final-project-cbecs')

#stratify training set
set.seed(20)
train_test_list <- createDataPartition(y=nn_input_pba_labels, 
                                       p=0.8, 
                                       list = FALSE)
train_test_df <- nn_input_df %>% 
  bind_cols(PBA=nn_input_pba_labels) %>% 
  slice(train_test_list)

train_test_labels <- (train_test_df %>% 
                        slice(train_test_list) %>% 
                        select(ELBTU))$ELBTU

set.seed(20)
train_list <- createDataPartition(y=train_test_df$PBA,
                                  p=0.8,
                                  list=FALSE)
train_raw_df <- train_test_df %>% 
  slice(train_list) %>% 
  select(-PBA, -ELBTU)

#Add points by jittering
scale_factor <- 0.5
set.seed(20)
random_rows <- runif(n=nrow(train_raw_df)*scale_factor, 
                     min=1, 
                     max=nrow(train_raw_df))

train_df <- train_raw_df[random_rows,] %>% 
  mutate_at(vars(one_of(numeric_cols)), funs(jitter(., factor = 1))) %>% 
  bind_rows(train_raw_df)

train_raw_labels <- (train_test_df %>% 
                   slice(train_list) %>% 
                   select(ELBTU))$ELBTU

train_labels <- append(train_raw_labels[random_rows], train_raw_labels)

test_df <- train_test_df %>% 
  slice(-train_list) %>% 
  select(-PBA, -ELBTU)

test_labels <- (train_test_df %>% 
                  slice(-train_list) %>% 
                  select(ELBTU))$ELBTU

validation_df <- nn_input_df %>% 
  slice(-train_test_list) %>% 
  select(-ELBTU)

validation_labels <- (nn_input_df %>%
                        slice(-train_test_list) %>% 
                        select(ELBTU))$ELBTU

#Custom loss funcitons
custom_loss_func <- function(y_true, y_pred) {
  K <- backend()
  true_log <- K$log(K$clip(y_true, 1, 1000000000))
  pred_log <- K$log(K$clip(y_pred, 1, 1000000000))
  K$mean(tf$multiply(K$square(true_log - pred_log) / K$clip(true_log,1,1000000000), true_log))
}

#Custom metrics
percentage_metric <- custom_metric('percentage_metric', function(y_true, y_pred){
  K <- backend()
  K$mean(tf$multiply(K$abs(1 - K$abs(y_true - y_pred) / K$clip(y_true,0.1,1000)), y_true))
})

#Set variable size
num_vars <- 14
train_reduced_df <- train_df %>% select(one_of(variables_by_importance[1:num_vars]))
test_reduced_df <- test_df %>% select(one_of(variables_by_importance[1:num_vars]))