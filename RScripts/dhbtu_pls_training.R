source('RScripts/dhbtu_base_training.R')

#Initialize parallel processing on 2 cores
cl <- makePSOCKcluster(2)
registerDoParallel(cl)

control <- trainControl(index = cbecs_train_cv_list, 
                        method = 'repeatedcv', 
                        number = folds, 
                        repeats = 2, 
                        search = 'grid', 
                        verboseIter = TRUE)
tunegrid <- expand.grid(ncomp = seq(1, 20, 1))
pls_train <- caret::train(
  y = log(cbecs_dhbtu_encoded_center_scale_train_df$DHBTU + 1),
  x = cbecs_dhbtu_encoded_center_scale_train_df %>% select(-DHBTU),
  method='pls',
  metric='RMSE',
  tuneGrid=tunegrid,
  trControl=control
)

#save model
model_name <- 'ModelSaves/dhbtu_pls.RData'
save(pls_train, file = model_name)
put_object(file = model_name, 
           bucket = 'cuny-msds-final-project-cbecs', 
           object = model_name)
#stop cluster
stopCluster(cl)