source('RScripts/elbtu_base_training.R')

#Initialize parallel processing on 2 cores
cl <- makePSOCKcluster(2)
registerDoParallel(cl)

control <- trainControl(index = cbecs_train_cv_list, 
                        method = 'repeatedcv', 
                        number = folds, 
                        repeats = 1, 
                        search = 'grid', 
                        verboseIter = TRUE)
tunegrid <- expand.grid(fraction = seq(0.1, 0.5, .1))
l_train <- caret::train(
  y = log(cbecs_elbtu_encoded_center_scale_train_df$ELBTU + 1),
  x = cbecs_elbtu_encoded_center_scale_train_df %>% select(-ELBTU),
  method='lasso',
  metric='RMSE',
  tuneGrid=tunegrid,
  trControl=control,
  preProcess = c('zv')#,
  #weights = 1 / pmax(1, cbecs_elbtu_encoded_center_scale_train_df$ELBTU)
)

#save model
model_name <- 'ModelSaves/elbtu_l.RData'
save(l_train, file = model_name)
put_object(file = model_name, 
           bucket = 'cuny-msds-final-project-cbecs', 
           object = model_name, 
           multipart = TRUE)
#stop cluster
stopCluster(cl)