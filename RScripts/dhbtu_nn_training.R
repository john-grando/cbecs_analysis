#Train neural network model and save.

source('RScripts/dhbtu_base_training.R')

#Initialize parallel processing on 2 cores
cl <- makePSOCKcluster(2)
registerDoParallel(cl)

#Simple neural network training
control <- trainControl(index = cbecs_train_cv_list, 
                        method = 'repeatedcv', 
                        number = folds, 
                        repeats = 2, 
                        search = 'grid')
tunegrid <- expand.grid(size = seq(100, 200, 100), 
                        dropout = seq(0.3, 0.9, 0.3), 
                        batch_size = seq(1000, 4000, 1000), 
                        lr = seq(0.3, 0.9, 0.3),
                        rho = seq(0.3, 0.9, 0.3),
                        decay = seq(0.3, 0.9, 0.3), 
                        activation = c('relu'))
nn_train <- caret::train(
  y = cbecs_dhbtu_encoded_center_scale_train_df$DHBTU,
  x = cbecs_dhbtu_encoded_center_scale_train_df %>% select(-DHBTU),
  method='mlpKerasDropout',
  metric='RMSE',
  tuneGrid=tunegrid,
  trControl=control,
  preProcess = c('zv')
)

#save model
nn_model_name <- 'ModelSaves/dhbtu_nn.RData'
save(nn_train, file = nn_model_name)
put_object(file = nn_model_name, 
           bucket = 'cuny-msds-final-project-cbecs', 
           object = nn_model_name)
#stop cluster
stopCluster(cl)