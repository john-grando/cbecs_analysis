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
s3load('ModelSaves/elbtu_nn_input_psf.RData', bucket = 'cuny-msds-final-project-cbecs')
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
                        select(ELBTUPerSf))$ELBTUPerSf


set.seed(20)
train_list <- createDataPartition(y=train_test_df$PBA,
                                  p=0.8,
                                  list=FALSE)
train_df <- train_test_df %>% 
  slice(train_list) %>% 
  select(-PBA, -ELBTUPerSf)

train_labels <- (train_test_df %>% 
                   slice(train_list) %>% 
                   select(ELBTUPerSf))$ELBTUPerSf

test_df <- train_test_df %>% 
  slice(-train_list) %>% 
  select(-PBA, -ELBTUPerSf)

test_labels <- (train_test_df %>% 
                  slice(-train_list) %>% 
                  select(ELBTUPerSf))$ELBTUPerSf

validation_df <- nn_input_df %>% 
  slice(-train_test_list) %>% 
  select(-ELBTUPerSf)

validation_labels <- (nn_input_df %>%
                        slice(-train_test_list) %>% 
                        select(ELBTUPerSf))$ELBTUPerSf

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

#Select model parameters
model <- model_selector(model_n = '3', df = train_df, n_dropout=0.6, n_units=200, n_l = 0)
batch_size <- 150
optimizer_func <- keras::optimizer_rmsprop(lr=0.001)
loss_func <- keras::loss_mean_squared_logarithmic_error
