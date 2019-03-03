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
tunegrid <- expand.grid(nvmax = seq(10, 100, 10))
lp_train <- caret::train(
  y = cbecs_elbtu_encoded_center_scale_train_df$ELBTU,
  x = cbecs_elbtu_encoded_center_scale_train_df %>% select(-ELBTU),
  method='leapForward',
  metric='RMSE',
  tuneGrid=tunegrid,
  trControl=control,
  preProcess = c('zv')
)

#save model
model_name <- 'ModelSaves/elbtu_lp.RData'
save(lp_train, file = model_name)
put_object(file = model_name, 
           bucket = 'cuny-msds-final-project', 
           object = model_name, 
           multipart = TRUE)
#stop cluster
stopCluster(cl)