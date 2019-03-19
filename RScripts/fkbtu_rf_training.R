source('RScripts/fkbtu_base_training.R')

#Initialize parallel processing on 2 cores
cl <- makePSOCKcluster(2)
registerDoParallel(cl)

control <- trainControl(index = cbecs_train_cv_list, 
                        method = 'repeatedcv', 
                        number = folds, 
                        repeats = 2, 
                        search = 'grid', 
                        verboseIter = TRUE)
tunegrid <- expand.grid(mtry = seq(50, 350, 100))
rf_train <- caret::train(
  y = cbecs_fkbtu_encoded_train_df$FKBTU,
  x = cbecs_fkbtu_encoded_train_df %>% select(-FKBTU),
  method='rf',
  metric='RMSE',
  tuneGrid=tunegrid,
  trControl=control,
  maxnodes=100,
  importance = TRUE
)

#save model
model_name <- 'ModelSaves/fkbtu_rf.RData'
save(rf_train, file = model_name)
put_object(file = model_name, 
           bucket = 'cuny-msds-final-project-cbecs', 
           object = model_name)
#stop cluster
stopCluster(cl)