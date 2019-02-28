source('RScripts/elbtu_base_training.R')

#Initialize parallel processing on 2 cores
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

control <- trainControl(index = cbecs_train_cv_list, 
                        method = 'repeatedcv', 
                        number = folds, 
                        repeats = 1, 
                        search = 'grid', 
                        verboseIter = TRUE)
tunegrid <- expand.grid(fraction = seq(0.4, 0.5, .1))
l_train <- caret::train(
  y = cbecs_elbtu_encoded_center_scale_train_df$ELBTUPerSf,
  x = cbecs_elbtu_encoded_center_scale_train_df %>% select(-ELBTUPerSf),
  method='lasso',
  metric='RMSE',
  tuneGrid=tunegrid,
  trControl=control,
  preProcess = c('zv')#,
  #weights = 1 / (cbecs_elbtu_encoded_center_scale_train_df$ELBTUPerSf)
)

#save model
model_name <- 'ModelSaves/elbtu_l.RData'
save(l_train, file = model_name)
put_object(file = model_name, 
           bucket = 'cuny-msds-final-project', 
           object = model_name, 
           multipart = TRUE)
#stop cluster
stopCluster(cl)