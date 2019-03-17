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

for(i in 1:2){
  nn_alt_df <- nn_input_df
  nn_alt_labels <- nn_input_pba_labels
  nn_jitter_input_df <- nn_input_df %>% 
    select(one_of(numeric_cols)) %>% 
    mutate_at(vars(numeric_cols), funs(jitter(., factor = 1)))
  nn_alt_df <- rbind(nn_alt_df, nn_jitter_input_df)
  nn_alt_labels <- rbind(nn_alt_labels, nn_input_pba_labels)
}


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

#train_summary_weight <- train_test_df %>% slice(train_list) %>% select(PBA) %>% group_by(PBA) %>% summarize(count = n())

#train_weight_tmp <- as.matrix(train_test_df %>% slice(train_list) %>% select(PBA) %>% left_join(train_summary_weight, by =c('PBA' = 'PBA')) %>% select(count))

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
