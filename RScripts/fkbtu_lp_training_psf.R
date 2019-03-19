source('RScripts/fkbtu_base_training_psf.R')

#Initialize parallel processing on 2 cores
cl <- makePSOCKcluster(2)
registerDoParallel(cl)

control <- trainControl(index = cbecs_train_cv_list, 
                        method = 'repeatedcv', 
                        number = folds, 
                        repeats = 2, 
                        search = 'grid', 
                        verboseIter = TRUE)
tunegrid <- expand.grid(nvmax = seq(10, 100, 10))
lp_train <- caret::train(
  y = log(cbecs_fkbtu_encoded_center_scale_train_df$FKBTUPerSf + 1),
  x = cbecs_fkbtu_encoded_center_scale_train_df %>% select(-FKBTUPerSf),
  method='leapForward',
  metric='RMSE',
  tuneGrid=tunegrid,
  trControl=control,
  preProcess = c('zv')
)

#save model
model_name <- 'ModelSaves/fkbtu_lp_psf.RData'
save(lp_train, file = model_name)
put_object(file = model_name, 
           bucket = 'cuny-msds-final-project-cbecs', 
           object = model_name)
#stop cluster
stopCluster(cl)