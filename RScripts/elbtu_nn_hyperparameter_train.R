
#Set up base and compile
source('RScripts/elbtu_nn_base_model.R')
#Get model types
source('RScripts/elbtu_nn_model_functions_build.R')

#Hyperparameter training
n_folds <- 4
folds <- createFolds(y = train_test_list, k=n_folds, list=FALSE)

hyper_list <- list()
hyper_list$dropout <- seq(0, 0.6, 0.3)
hyper_list$units <- seq(10, 110, 25)
hyper_list$regularizer <- seq(0, 0.2, 0.1)
hyper_list$model <- seq(0,3,1)
hyper_list$batch <- seq(10, 210, 50)
hyper_list$loss <- list(list(name = 'mse', func = 'mse'), 
                        list(name = 'msle', func = keras::loss_mean_squared_logarithmic_error))
hyper_list$opt <- list(list(name = 'rmsprop_lr_001', func = keras::optimizer_rmsprop(lr = 0.001)),
                    list(name = 'sgd_lr_01', func = keras::optimizer_sgd(lr=0.01)))


#Initialize parallel processing on 2 cores
cl <- makeCluster(2)
registerDoMC(2)

#Run model
epochs <- 200
hyper_results <- data.frame()
for (o in 1:length(hyper_list$opt)) {
  for (l in 1:length(hyper_list$loss)){
    for (m in hyper_list$model) {
      for (r in hyper_list$regularizer) {
        for (d in hyper_list$dropout) {
          for (u in hyper_list$units) {
            for (b in hyper_list$batch) {
              tmp_df <- foreach(f = 1:n_folds,
                                .combine = rbind) %dopar% {
                                  #print(paste('running dropout, units, regularizer, fold:',d,u,r,f, sep=' '))
                                  ind <- which(folds == f)
                                  cv_train_df <- train_test_df %>% slice(-ind) %>% select(-PBA, -ELBTU)
                                  cv_train_y <- train_test_df %>% slice(-ind) %>% select(ELBTU)
                                  cv_test_df <- train_test_df %>% slice(ind) %>% select(-PBA, -ELBTU)
                                  cv_test_y <- train_test_df %>% slice(ind) %>% select(ELBTU)
                                  cv_model_t <- model_selector(model_n = m, df = train_df, n_dropout = d, n_units = u, n_l=r)
                                  cv_model_t %>% compile(
                                    loss = hyper_list$loss[[l]]$func,
                                    optimizer = hyper_list$opt[[o]]$func,
                                    metrics = list("mean_absolute_error", percentage_metric))
                                  history <- cv_model_t %>% fit(
                                    as.matrix(cv_train_df),
                                    as.matrix(cv_train_y),
                                    epochs = epochs,
                                    validation_data = list(as.matrix(cv_test_df), as.matrix(cv_test_y)),
                                    batch_size = b,
                                    verbose = 0)
                                  loss_val <- tail(history$metrics$val_loss,1)
                                  mae_val <- tail(history$metrics$val_mean_absolute_error,1)
                                  pm_val <- tail(history$metrics$percentage_metric,1)
                                  #hyper_results <- rbind(hyper_results, data.frame(dropout=d, units=u, fold=f, loss= loss_val, mae = mae_val, pm = pm_val))
                                  data.frame(loss_f=hyper_list$loss[[l]]$name,
                                             opt=hyper_list$opt[[o]]$name,
                                             model=m, 
                                             dropout=d, 
                                             units=u, 
                                             reg=r, 
                                             batch=b,
                                             fold=f, 
                                             loss= loss_val, 
                                             mae = mae_val, 
                                             pm = pm_val)
                                }
              print(paste('finished running model -  loss:', hyper_list$loss[[l]]$name, 
                          'optimizer:', hyper_list$opt[[o]]$name, 
                          'model:', m, 
                          'regularizer:', r, 
                          'droput:', d,
                          'units:', u, 
                          'batch:', b, sep = " "))
              hyper_results <- rbind(hyper_results, tmp_df)
              print(hyper_results %>% 
                    group_by(loss_f, opt, model, dropout, units, reg, batch) %>% 
                    summarize(loss = mean(loss), 
                              mae = mean(mae), 
                              pm = mean(pm))%>% 
                    arrange(mae) %>% 
                    head(10))
            }
          }
        }
      }
    }
  }
}
#save hyperparameter df
#model_name <- 'ModelSaves/elbtu_nn_hyperparameter_results.RData'
#save(hyper_results, file = model_name)
#put_object(file = model_name, 
#           bucket = 'cuny-msds-final-project', 
#           object = model_name, 
#           multipart = TRUE)

#Stop parallel processing
stopCluster(cl)

