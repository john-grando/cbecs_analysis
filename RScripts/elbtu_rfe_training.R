source('RScripts/elbtu_base_training.R')

#Initialize parallel processing on 2 cores
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

ctrl <- rfeControl(functions = treebagFuncs,
                   index = cbecs_train_cv_list, 
                   method = 'repeatedcv', 
                   number = folds, 
                   repeats = 1,
                   verbose = TRUE)

rfe_train <- rfe(y = cbecs_elbtu_encoded_center_scale_train_df$ELBTUPerSf,
                 x = cbecs_elbtu_encoded_center_scale_train_df %>% select(-ELBTUPerSf),
                 sizes = c(1:20, seq(50, 700, 50)),
                 rfeControl = ctrl)

#save model
model_name <- 'ModelSaves/elbtu_rfe.RData'
save(rfe_train, file = model_name)
put_object(file = model_name, 
           bucket = 'cuny-msds-final-project', 
           object = model_name, 
           multipart = TRUE)

#stop cluster
stopCluster(cl)