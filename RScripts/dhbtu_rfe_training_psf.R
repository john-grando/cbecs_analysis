source('RScripts/dhbtu_base_training_psf.R')

#Initialize parallel processing on 2 cores
cl <- makePSOCKcluster(2)
registerDoParallel(cl)

ctrl <- rfeControl(functions = treebagFuncs,
                   index = cbecs_train_cv_list, 
                   method = 'repeatedcv', 
                   number = folds, 
                   repeats = 2,
                   verbose = TRUE)

rfe_train <- rfe(y = cbecs_dhbtu_encoded_center_scale_train_df$DHBTUPerSf,
                 x = cbecs_dhbtu_encoded_center_scale_train_df %>% select(-DHBTUPerSf),
                 sizes = c(seq(50, 700, 50)),
                 rfeControl = ctrl)

#save model
model_name <- 'ModelSaves/dhbtu_rfe_psf.RData'
save(rfe_train, file = model_name)
put_object(file = model_name, 
           bucket = 'cuny-msds-final-project-cbecs', 
           object = model_name)

#stop cluster
stopCluster(cl)
