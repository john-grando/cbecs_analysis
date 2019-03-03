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

#Load input data and assign as train/test/validation
s3load('ModelSaves/elbtu_nn_input.RData', bucket = 'cuny-msds-final-project')
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
train_df <- train_test_df %>% 
  slice(train_list) %>% 
  select(-PBA, -ELBTU)

train_labels <- (train_test_df %>% 
                   slice(train_list) %>% 
                   select(ELBTU))$ELBTU

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
  second_y <- tf$multiply(K$relu(y_true), 0.01)
  K$mean(tf$multiply(K$square(y_pred - y_true) / K$clip(y_true,0.1,1000), second_y))
}

#Custom metrics
percentage_metric <- custom_metric('percentage_metric', function(y_true, y_pred){
  K <- backend()
  K$mean(tf$multiply(K$abs(1 - K$abs(y_true - y_pred) / K$clip(y_true,0.1,1000)), y_true))
})
