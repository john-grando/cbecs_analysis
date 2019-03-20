#function for cleaning and transforming source data
source('RScripts/cbecs_2012_clean_transform.R')
#libraries
library(aws.s3)
library(aws.signature)
library(gridExtra)
#library(factoextra)
library(pls)
library(randomForest)
library(inTrees)
library(doParallel)
library(doMC)
library(elasticnet)
library(keras)
library(tibble)
library(RSNNS)
library(car)
library(nnet)

#Load and transform
cbecs_raw_df <- s3read_using(read.csv, 
                             bucket = 'cuny-msds-final-project-cbecs',
                             object = '2012_public_use_data_aug2016.csv',
                             header = TRUE)
cbecs_dfs <- clean_encode_cbecs(cbecs_raw_df)

#remove buildings that know the don't use electricity and outliers
cbecs_ng_encoded_df <- cbecs_dfs$encoded_df %>% 
  filter(!is.na(NGBTU)) %>%
  filter(NGUSED.1==1)
cbecs_ng_cleaned_df <- cbecs_dfs$clean_df %>% 
  filter(!is.na(NGBTU)) %>%
  filter(NGUSED==1)

zero_vals <- preProcess(cbecs_ng_encoded_df, method = c('zv'))$method$remove

if(is.null(zero_vals)){
  tmp_df <- cbecs_ng_encoded_df
}

if(!is.null(zero_vals)){
  tmp_df <- cbecs_ng_encoded_df %>% 
    select(-one_of(preProcess(., method = c('zv'))$method$remove))
}

#Remove highly correlated pairs
cbecs_ngbtu_encoded_df <- tmp_df %>% 
  #remove response columns
  select(-one_of(cbecs_dfs$response_cols)) %>%
  #remove pba
  #select(-matches('^PBA\\.')) %>% 
  select(-one_of(findCorrelation(cor(.), cutoff=0.75, names = TRUE))) %>%
  #remove remaining pba values then reinsert all
  #select(-matches('^PBAPLUS')) %>% 
  #bind_cols(cbecs_ng_encoded_df %>% select(matches('^PBAPLUS'))) %>% 
  bind_cols(NGBTU = cbecs_ng_encoded_df %>% select(NGBTU))

cbecs_ngbtu_encoded_numerics_cols <- colnames(
  cbecs_ngbtu_encoded_df %>% 
    select(one_of(cbecs_dfs$encoded_numeric_cols)) %>% 
    select(-NGBTU)) 
cbecs_ngbtu_encoded_non_numerics_cols <- colnames(
  cbecs_ngbtu_encoded_df %>% 
    select(one_of(cbecs_dfs$encoded_non_numeric_cols)))

#Make offset for boxcox
cbecs_ngbtu_encoded_offset_df <- cbecs_ngbtu_encoded_df %>% 
  mutate_at(vars(cbecs_ngbtu_encoded_numerics_cols), list(~. + 0.5 * min(.[which(. > 0)])))

#center and scale only numeric columns in data set, not encoder columns
ngbtu_pre_process <- preProcess(
  cbecs_ngbtu_encoded_offset_df, 
  method = list(
    BoxCox = cbecs_ngbtu_encoded_numerics_cols,
    center = cbecs_ngbtu_encoded_numerics_cols, 
    scale = cbecs_ngbtu_encoded_numerics_cols))

#Apply transformations to dataframe
cbecs_ngbtu_encoded_center_scale_df <- predict(ngbtu_pre_process, 
                                               cbecs_ngbtu_encoded_df %>% 
                                                 mutate_at(vars(cbecs_ngbtu_encoded_numerics_cols), 
                                                           list(~. + 0.5 * min(.[which(. > 0)]))))

#Train/Test split
set.seed(20)
cbecs_train_list <- createDataPartition(y= cbecs_ng_cleaned_df$PBAPLUS, p=0.8, list = FALSE) 
cbecs_ngbtu_encoded_train_df <- cbecs_ngbtu_encoded_df[cbecs_train_list,]
cbecs_ngbtu_encoded_test_df <- cbecs_ngbtu_encoded_df[-cbecs_train_list,]
cbecs_ngbtu_encoded_center_scale_train_df <- cbecs_ngbtu_encoded_center_scale_df[cbecs_train_list,]
cbecs_ngbtu_encoded_center_scale_test_df <- cbecs_ngbtu_encoded_center_scale_df[-cbecs_train_list,]
#stratify training set
folds <- 5
set.seed(20)
cbecs_train_cv_list <- createFolds(y=cbecs_ng_cleaned_df[cbecs_train_list,]$PBAPLUS, 
                                   k = folds, 
                                   returnTrain = TRUE)
