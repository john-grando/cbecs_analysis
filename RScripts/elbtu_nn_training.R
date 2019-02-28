#Train neural network model and save.

source('RScripts/elbtu_base_training.R')

#Initialize parallel processing on 2 cores
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

#Simple neural network training
control <- trainControl(index = cbecs_train_cv_list, 
                        method = 'repeatedcv', 
                        number = folds, 
                        repeats = 1, 
                        search = 'grid')
tunegrid <- expand.grid(size = seq(100, 200, 100), 
                        dropout = seq(0.3, 0.9, 0.3), 
                        batch_size = seq(1000, 4000, 1000), 
                        lr = seq(0.3, 0.9, 0.3),
                        rho = seq(0.3, 0.9, 0.3),
                        decay = seq(0.3, 0.9, 0.3), 
                        activation = c('relu'))
nn_train <- caret::train(
  y = cbecs_elbtu_encoded_center_scale_train_df$ELBTUPerSf,
  x = cbecs_elbtu_encoded_center_scale_train_df %>% select(-ELBTUPerSf),
  method='mlpKerasDropout',
  metric='RMSE',
  tuneGrid=tunegrid,
  trControl=control,
  preProcess = c('zv')
)

#save model
nn_model_name <- 'ModelSaves/elbtu_nn.RData'
save(nn_train, file = nn_model_name)
put_object(file = nn_model_name, 
           bucket = 'cuny-msds-final-project', 
           object = nn_model_name, 
           multipart = TRUE)
#stop cluster
stopCluster(cl)