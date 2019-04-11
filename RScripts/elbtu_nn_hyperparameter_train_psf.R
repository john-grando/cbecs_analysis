#Set up base and compile
source('RScripts/elbtu_nn_base_model_psf.R')
#Get model types
source('RScripts/elbtu_nn_model_functions_build.R')

#if hyperparameter df exists then use it
if(head_object('ModelSaves/elbtu_nn_hyperparameter_results_psf.RData', bucket='cuny-msds-final-project-cbecs')[1]==TRUE){
  s3load('ModelSaves/elbtu_nn_hyperparameter_results_psf.RData', bucket = 'cuny-msds-final-project-cbecs')
}
if(head_object('ModelSaves/elbtu_nn_hyperparameter_results_psf.RData', bucket='cuny-msds-final-project-cbecs')[1]==FALSE){
  hyper_results <- data.frame()
}

#Hyperparameter training
n_folds <- 6
set.seed(20)
folds <- createFolds(y = train_test_list, k=n_folds, list=FALSE)

hyper_list <- list()
hyper_list$dropout <- seq(0.3, 0.9, 0.3)
hyper_list$units <- seq(200, 600, 200)
hyper_list$regularizer <- seq(0, 0.3, 0.15)
hyper_list$model <- seq(3,5,1)
hyper_list$batch <- seq(150, 350, 100)
hyper_list$loss <- list(
      #list(name = 'mse', func = 'mse'), 
      list(name = 'msle', func = keras::loss_mean_squared_logarithmic_error),
      list(name = 'cusom_loss_func', func = custom_loss_func)
)
hyper_list$opt <- list(
  #list(name = 'rmsprop_lr_00005', func = keras::optimizer_rmsprop(lr = 0.00005)),
  list(name = 'rmsprop_lr_0001', func = keras::optimizer_rmsprop(lr = 0.0001)),
  list(name = 'rmsprop_lr_0005', func = keras::optimizer_rmsprop(lr = 0.0005)),
  list(name = 'rmsprop_lr_001', func = keras::optimizer_rmsprop(lr = 0.001)),
  list(name = 'sgd_lr_005_m_0_d_0_n_T', func = keras::optimizer_sgd(lr=0.005, momentum=0.0, decay=0.0, nesterov=TRUE)),
  #list(name = 'sgd_lr_01_m_0_d_0_n_T', func = keras::optimizer_sgd(lr=0.01, momentum=0.0, decay=0.0, nesterov=TRUE)),
  #list(name = 'sgd_lr_015_m_0_d_0_n_T', func = keras::optimizer_sgd(lr=0.015, momentum=0.0, decay=0.0, nesterov=TRUE)),
  list(name = 'sgd_lr_005_m_0_d_0_n_F', func = keras::optimizer_sgd(lr=0.005, momentum=0.0, decay=0.0, nesterov=FALSE)),
  #list(name = 'sgd_lr_01_m_5_d_0_n_T', func = keras::optimizer_sgd(lr=0.01, momentum=0.5, decay=0.0, nesterov=TRUE)),
  #list(name = 'sgd_lr_015_m_9_d_0_n_T', func = keras::optimizer_sgd(lr=0.015, momentum=0.9, decay=0.0, nesterov=TRUE)),
  #list(name = 'sgd_lr_005_m_0_d_1E6_n_T', func = keras::optimizer_sgd(lr=0.005, momentum=0.0, decay=1e-6, nesterov=TRUE)),
  list(name = 'sgd_lr_01_m_5_d_1E3_n_T', func = keras::optimizer_sgd(lr=0.01, momentum=0.5, decay=1e-3, nesterov=TRUE)),
  #list(name = 'sgd_lr_015_m_9_d_1_n_T', func = keras::optimizer_sgd(lr=0.015, momentum=0.9, decay=0.1, nesterov=TRUE)),
  #list(name = 'sgd_lr_005_m_0_d_1E6_n_T', func = keras::optimizer_sgd(lr=0.005, momentum=0.0, decay=1e-6, nesterov=TRUE)),
  #list(name = 'sgd_lr_01_m_0_d_1E3_n_T', func = keras::optimizer_sgd(lr=0.01, momentum=0.0, decay=1e-3, nesterov=TRUE)),
  list(name = 'sgd_lr_015_m_0_d_1_n_T', func = keras::optimizer_sgd(lr=0.015, momentum=0.0, decay=0.1, nesterov=TRUE))
)

#Initialize parallel processing on 2 cores
#cl <- makeCluster(2)
registerDoMC(3)

#Run model
epochs <- 1000

for (o in 1:length(hyper_list$opt)) {
  for (l in 1:length(hyper_list$loss)){
    for (m in hyper_list$model) {
      for (r in hyper_list$regularizer) {
        for (d in hyper_list$dropout) {
          for (u in hyper_list$units) {
            for (b in hyper_list$batch) {
              for(v in c(seq(0,length(variables_by_importance),300), 
                         length(variables_by_importance)-100,
                         length(variables_by_importance)-50,
                         length(variables_by_importance)-10,
                         length(variables_by_importance)-2
              )) {
              tmp_df <- foreach(f = 1:n_folds,
                                .combine = rbind) %dopar% {
                                  #print(paste('running dropout, units, regularizer, fold:',d,u,r,f, sep=' '))
                                  ind <- which(folds == f)
                                  cv_train_df <- train_test_df %>% 
                                    slice(-ind) %>% 
                                    select(-PBA, -ELBTUPerSf) %>% 
                                    select(variables_by_importance[1:(length(variables_by_importance)-v)])
                                  cv_train_y <- train_test_df %>% 
                                    slice(-ind) %>% 
                                    select(ELBTUPerSf)
                                  cv_test_df <- train_test_df %>% 
                                    slice(ind) %>% 
                                    select(-PBA, -ELBTUPerSf) %>% 
                                    select(variables_by_importance[1:(length(variables_by_importance)-v)])
                                  cv_test_y <- train_test_df %>% 
                                    slice(ind) %>% 
                                    select(ELBTUPerSf)
                                  cv_model_t <- model_selector(model_n = m, 
                                                               df = train_df %>% 
                                                                 select(variables_by_importance[1:(length(variables_by_importance)-v)]), 
                                                               n_dropout = d, 
                                                               n_units = u, 
                                                               n_l=r)
                                  cv_model_t %>% compile(
                                    loss = hyper_list$loss[[l]]$func,
                                    optimizer = hyper_list$opt[[o]]$func,
                                    metrics = list("mean_absolute_error", 
                                                   "mean_squared_error", 
                                                   'mean_squared_logarithmic_error',
                                                   mean_absolute_percentage_error
                                                   ))
                                  history <- cv_model_t %>% fit(
                                    as.matrix(cv_train_df),
                                    as.matrix(cv_train_y),
                                    epochs = epochs,
                                    validation_data = list(as.matrix(cv_test_df), as.matrix(cv_test_y)),
                                    batch_size = b,
                                    verbose = 0)
                                  loss_val <- tail(history$metrics$val_loss,1)
                                  mae_val <- tail(history$metrics$val_mean_absolute_error,1)
                                  msle_val <- tail(history$metrics$val_mean_squared_logarithmic_error,1)
			                            mse_val <- tail(history$metrics$val_mean_squared_error,1)
                                  mape_val <- tail(history$metrics$mean_absolute_percentage_error,1)
                                  #hyper_results <- rbind(hyper_results, data.frame(dropout=d, units=u, fold=f, loss= loss_val, mae = mae_val, pm = pm_val))
                                  data.frame(num_vars=length(variables_by_importance)-v,
                                             loss_f=hyper_list$loss[[l]]$name,
                                             opt=hyper_list$opt[[o]]$name,
                                             model=m, 
                                             dropout=d, 
                                             units=u, 
                                             reg=r, 
                                             batch=b,
                                             fold=f, 
                                             loss= loss_val, 
                                             mae = mae_val, 
                                             msle = msle_val,
				                                     mse = mse_val,
                                             mape = mape_val,
				                                     run_time = Sys.time())
                                }
              print(paste('finished running model - variables:', length(variables_by_importance)-v,
                          'loss:', hyper_list$loss[[l]]$name, 
                          'optimizer:', hyper_list$opt[[o]]$name, 
                          'model:', m, 
                          'regularizer:', r, 
                          'droput:', d,
                          'units:', u, 
                          'batch:', b, sep = " "))
              hyper_results <- rbind(hyper_results, tmp_df)
              print(hyper_results %>% 
                    group_by(num_vars, loss_f, opt, model, dropout, units, reg, batch) %>% 
                    summarize(mae = mean(mae), 
                              mape = mean(mape),
			                        mse = mean(mse),
			                        msle = mean(msle),
			                        loss = mean(loss)
			                        )%>% 
                    arrange(mape) %>% 
                    head(10))
              }
            }
          }
        }
      }
    }
    print('intermittent print to df')
    #save hyperparameter df
    model_name <- 'ModelSaves/elbtu_nn_hyperparameter_results_psf.RData'
    save(hyper_results, file = model_name)
    put_object(file = model_name, 
               bucket = 'cuny-msds-final-project-cbecs', 
               object = model_name)
  }
}
#save hyperparameter df
model_name <- 'ModelSaves/elbtu_nn_hyperparameter_results_psf.RData'
save(hyper_results, file = model_name)
put_object(file = model_name, 
           bucket = 'cuny-msds-final-project-cbecs', 
           object = model_name)

#Stop parallel processing
#stopCluster(cl)