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
s3load('ModelSaves/ngbtu_nn_input_psf.RData', bucket = 'cuny-msds-final-project-cbecs')

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
                        select(NGBTUPerSf))$NGBTUPerSf

set.seed(20)
train_list <- createDataPartition(y=train_test_df$PBA,
                                  p=0.8,
                                  list=FALSE)
train_raw_df <- train_test_df %>% 
  slice(train_list) %>% 
  select(-PBA, -NGBTUPerSf)

#Add points by jittering
scale_factor <- 0.5
set.seed(20)
random_rows <- runif(n=nrow(train_raw_df)*scale_factor, 
                     min=1, 
                     max=nrow(train_raw_df))

train_df <- train_raw_df[random_rows,] %>% 
  mutate_at(vars(one_of(numeric_cols)), list(~jitter(., factor = 1))) %>% 
  bind_rows(train_raw_df)

train_raw_labels <- (train_test_df %>% 
                       slice(train_list) %>% 
                       select(NGBTUPerSf))$NGBTUPerSf

train_labels <- append(train_raw_labels[random_rows], train_raw_labels)

test_df <- train_test_df %>% 
  slice(-train_list) %>% 
  select(-PBA, -NGBTUPerSf)

test_labels <- (train_test_df %>% 
                  slice(-train_list) %>% 
                  select(NGBTUPerSf))$NGBTUPerSf

validation_df <- nn_input_df %>% 
  slice(-train_test_list) %>% 
  select(-NGBTUPerSf)

validation_labels <- (nn_input_df %>%
                        slice(-train_test_list) %>% 
                        select(NGBTUPerSf))$NGBTUPerSf

validation_sqft_values <- sqft_values[-train_test_list]

#Custom loss funcitons
custom_loss_func <- function(y_true, y_pred) {
  K <- backend()
  K$mean(K$abs(y_true - y_pred) / (K$relu(y_true)+1)) * 100
}

#Custom metrics
mean_absolute_percentage_error <- custom_metric('mean_absolute_percentage_error', function(y_true, y_pred){
  K <- backend()
  K$mean(K$abs(y_true - y_pred) / (K$relu(y_true)+1)) * 100
})

#Set variable size
num_vars <- 100
train_reduced_df <- train_df %>% select(one_of(variables_by_importance[1:num_vars]))
test_reduced_df <- test_df %>% select(one_of(variables_by_importance[1:num_vars]))