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
cbecs_el_encoded_df <- cbecs_dfs$encoded_df %>% 
  filter(!is.na(ELBTU)) %>%
  #e05 is used instead of e08 because the data has been converted to mmBTU
  filter(ELBTU<7E05)
  #filter(ELUSED.1==1)
cbecs_el_cleaned_df <- cbecs_dfs$clean_df %>% 
  filter(!is.na(ELBTU)) %>%
  #e05 is used instead of e08 because the data has been converted to mmBTU
  filter(ELBTU<7E05) #%>% 
  #filter(ELUSED==1)

zero_vals <- preProcess(cbecs_el_encoded_df, method = c('zv'))$method$remove

if(is.null(zero_vals)){
  tmp_df <- cbecs_el_encoded_df
}

if(!is.null(zero_vals)){
  tmp_df <- cbecs_el_encoded_df %>% 
    select(-one_of(preProcess(., method = c('zv'))$method$remove))
}

#Remove highly correlated pairs
cbecs_elbtu_encoded_df <- tmp_df %>% 
  #remove response columns
  select(-one_of(cbecs_dfs$response_cols)) %>%
  #remove pba
  #select(-matches('^PBA\\.')) %>% 
  select(-one_of(findCorrelation(cor(.), cutoff=0.75, names = TRUE))) %>%
  #remove remaining pba values then reinsert all
  #select(-matches('^PBAPLUS')) %>% 
  #bind_cols(cbecs_el_encoded_df %>% select(matches('^PBAPLUS'))) %>% 
  bind_cols(ELBTU = cbecs_el_encoded_df %>% select(ELBTU))

cbecs_elbtu_encoded_numerics_cols <- colnames(
  cbecs_elbtu_encoded_df %>% 
    select(one_of(cbecs_dfs$encoded_numeric_cols)) %>% 
    select(-ELBTU)) 
cbecs_elbtu_encoded_non_numerics_cols <- colnames(
  cbecs_elbtu_encoded_df %>% 
    select(one_of(cbecs_dfs$encoded_non_numeric_cols)))

#Make offset for boxcox
cbecs_elbtu_encoded_offset_df <- cbecs_elbtu_encoded_df %>% 
  #Add 50% of the minimum value to the lowest (0) value.
  mutate_at(vars(cbecs_elbtu_encoded_numerics_cols), funs(. + 0.5 * min(.[which(. > 0)])))

#center and scale only numeric columns in data set, not encoder columns
elbtu_pre_process <- preProcess(
  cbecs_elbtu_encoded_offset_df, 
  method = list(
    BoxCox = cbecs_elbtu_encoded_numerics_cols,
    center = cbecs_elbtu_encoded_numerics_cols, 
    scale = cbecs_elbtu_encoded_numerics_cols))

#Apply transformations to dataframe
cbecs_elbtu_encoded_center_scale_df <- predict(elbtu_pre_process, 
                                               cbecs_elbtu_encoded_df %>% 
                                                 mutate_at(vars(cbecs_elbtu_encoded_numerics_cols), 
                                                           funs(. + 0.5 * min(.[which(. > 0)]))))

#Train/Test split
set.seed(20)
cbecs_train_list <- createDataPartition(y= cbecs_el_cleaned_df$PBAPLUS, p=0.8, list = FALSE) 
cbecs_elbtu_encoded_train_df <- cbecs_elbtu_encoded_df[cbecs_train_list,]
cbecs_elbtu_encoded_test_df <- cbecs_elbtu_encoded_df[-cbecs_train_list,]
cbecs_elbtu_encoded_center_scale_train_df <- cbecs_elbtu_encoded_center_scale_df[cbecs_train_list,]
cbecs_elbtu_encoded_center_scale_test_df <- cbecs_elbtu_encoded_center_scale_df[-cbecs_train_list,]
#stratify training set
folds <- 5
set.seed(20)
cbecs_train_cv_list <- createFolds(y=cbecs_el_cleaned_df[cbecs_train_list,]$PBAPLUS, 
                                   k = folds, 
                                   returnTrain = TRUE)
