#Train various nn configurations on full epochs

#Set up base and compile
source('RScripts/ngbtu_nn_base_model_psf.R')

#if hyperparameter df exists then use it
if(head_object('ModelSaves/ngbtu_nn_full_set_test_results_psf.RData', bucket='cuny-msds-final-project-cbecs')[1]==TRUE){
  s3load('ModelSaves/ngbtu_nn_full_set_test_results_psf.RData', bucket = 'cuny-msds-final-project-cbecs')
}
if(head_object('ModelSaves/ngbtu_nn_full_set_test_results_psf.RData', bucket='cuny-msds-final-project-cbecs')[1]==FALSE){
  hyper_results <- data.frame()
  hyper_models <- list()
}

model_run <- function(model_n_f, n_dropout_f, n_units_f, n_l_f, loss_func_f, optimizer_func_f, batch_size_f, num_vars_f) {
  model <- model_selector(model_n = model_n_f, 
                          df = train_df %>% 
                            select(variables_by_importance[1:num_vars_f]), 
                          n_dropout=n_dropout_f, 
                          n_units=n_units_f, 
                          n_l = n_l_f)
  #Compile
  model %>% compile(
    loss = loss_func_f$func,
    optimizer = optimizer_func_f$func,
    metrics = list("mean_squared_error", "mean_squared_logarithmic_error", "mean_absolute_error", mean_absolute_percentage_error)
  )
  
  #Function for model building
  # Display training progress by printing a single dot for each completed epoch.
  print_dot_callback <- callback_lambda(
    on_epoch_end = function(epoch, logs) {
      if (epoch %% 100 == 0) cat("\n")
      cat(".")
    }
  )
  
  # The patience parameter is the amount of epochs to check for improvement.
  early_stop <- callback_early_stopping(monitor = "val_loss", patience = 200)
  
  #Run
  train_reduced_df <- train_df %>% select(one_of(variables_by_importance[1:num_vars_f]))
  test_reduced_df <- test_df %>% select(one_of(variables_by_importance[1:num_vars_f]))
  epochs <- 1000
  history <- model %>% fit(
    as.matrix(train_reduced_df),
    as.matrix(train_labels),
    epochs = epochs,
    validation_data = list(as.matrix(test_reduced_df), as.matrix(test_labels)),
    batch_size = batch_size_f,
    verbose = 0,
    callbacks = list(print_dot_callback, 
                     early_stop
    )
  )
  loss_val <- tail(history$metrics$val_loss,1)
  mae_val <- tail(history$metrics$val_mean_absolute_error,1)
  msle_val <- tail(history$metrics$val_mean_squared_logarithmic_error,1)
  mse_val <- tail(history$metrics$val_mean_squared_error,1)
  mape_val <- tail(history$metrics$val_mean_absolute_percentage_error,1)
  print('model run complete')
  #save hyperparameter df
  model_name <- 'ModelSaves/ngbtu_nn_full_set_test_results_psf.RData'
  tmp_df <- data.frame(
    model_number = model_n_f, 
    dropout = n_dropout_f, 
    units = n_units_f, 
    reg = n_l_f, 
    loss_func = loss_func_f$name, 
    opt = optimizer_func_f$name, 
    batch = batch_size_f, 
    num_vars = num_vars_f,
    loss= loss_val, 
    mae = mae_val, 
    msle = msle_val,
    mse = mse_val,
    mape = mape_val,
    run_time = Sys.time()
  )
  hyper_results <- rbind(
    hyper_results,
    tmp_df
  )
  print(hyper_results %>% arrange(mape) %>% head(10))
  hyper_model_name <- paste(model_n_f, 
                            n_dropout_f,
                            n_units_f, 
                            n_l_f, 
                            loss_func_f$name, 
                            optimizer_func_f$name, 
                            batch_size_f, 
                            num_vars_f, 
                            sep = '_')
  hyper_models[[hyper_model_name]] <- list(history = history)
  save(hyper_results, hyper_models, file = model_name)
  #Save summary
  put_object(file = model_name, 
             bucket = 'cuny-msds-final-project-cbecs', 
             object = model_name)
  print(paste0('Model completed and saved: ', hyper_model_name))
  return(list(hyper_results = hyper_results,
              hyper_models = hyper_models))
}

#Make list of models to run
run_list <- list(list(m_n='4', 
                        n_d=0.3, 
                        n_u=600, 
                        n_l=0.0, 
                        l_f=list(name = 'msle', func = keras::loss_mean_squared_logarithmic_error), 
                        o_f=list(name = 'rmsprop_0005', func = keras::optimizer_rmsprop(lr=0.0005)),
                        b_f=150,
                        n_v=791),
                 list(m_n='4', 
                      n_d=0.3, 
                      n_u=600, 
                      n_l=0.0, 
                      l_f=list(name = 'msle', func = keras::loss_mean_squared_logarithmic_error), 
                      o_f=list(name = 'rmsprop_0005', func = keras::optimizer_rmsprop(lr=0.0005)),
                      b_f=150,
                      n_v=491),
                   list(m_n='5', 
                        n_d=0.3, 
                        n_u=600, 
                        n_l=0, 
                        l_f=list(name = 'msle', func = keras::loss_mean_squared_logarithmic_error), 
                        o_f=list(name = 'rmsprop_0005', func = keras::optimizer_rmsprop(lr=0.0005)),
                        b_f=150,
                        n_v=191),
                   list(m_n='5', 
                        n_d=0.3, 
                        n_u=600, 
                        n_l=0, 
                        l_f=list(name = 'msle', func = keras::loss_mean_squared_logarithmic_error), 
                        o_f=list(name = 'rmsprop_0005', func = keras::optimizer_rmsprop(lr=0.0005)),
                        b_f=150,
                        n_v=100),
                 list(m_n='4', 
                      n_d=0.3, 
                      n_u=800, 
                      n_l=0, 
                      l_f=list(name = 'msle', func = keras::loss_mean_squared_logarithmic_error), 
                      o_f=list(name = 'rmsprop_0001', func = keras::optimizer_rmsprop(lr=0.0001)),
                      b_f=150,
                      n_v=100),
                 list(m_n='4', 
                      n_d=0.3, 
                      n_u=800, 
                      n_l=0, 
                      l_f=list(name = 'msle', func = keras::loss_mean_squared_logarithmic_error), 
                      o_f=list(name = 'rmsprop_0005', func = keras::optimizer_rmsprop(lr=0.0005)),
                      b_f=150,
                      n_v=100),
                   list(m_n='5', 
                        n_d=0.3, 
                        n_u=600, 
                        n_l=0, 
                        l_f=list(name = 'custom_loss', func = custom_loss_func), 
                        o_f=list(name = 'rmsprop_0001', func = keras::optimizer_rmsprop(lr=0.0001)),
                        b_f=150,
                        n_v=50),
                 list(m_n='5', 
                      n_d=0.3, 
                      n_u=600, 
                      n_l=0, 
                      l_f=list(name = 'msle', func = keras::loss_mean_squared_logarithmic_error), 
                      o_f=list(name = 'rmsprop_0001', func = keras::optimizer_rmsprop(lr=0.0001)),
                      b_f=150,
                      n_v=50),
                   list(m_n='5', 
                        n_d=0.3, 
                        n_u=600, 
                        n_l=0, 
                        l_f=list(name = 'custom_loss', func = custom_loss_func), 
                        o_f=list(name = 'rmsprop_0001', func = keras::optimizer_rmsprop(lr=0.0001)),
                        b_f=150,
                        n_v=10),
                 list(m_n='5', 
                      n_d=0.3, 
                      n_u=600, 
                      n_l=0, 
                      l_f=list(name = 'msle', func = keras::loss_mean_squared_logarithmic_error), 
                      o_f=list(name = 'rmsprop_0001', func = keras::optimizer_rmsprop(lr=0.0001)),
                      b_f=150,
                      n_v=10),
                   list(m_n='5', 
                        n_d=0.3, 
                        n_u=600, 
                        n_l=0, 
                        l_f=list(name = 'custom_loss', func = custom_loss_func), 
                        o_f=list(name = 'rmsprop_0001', func = keras::optimizer_rmsprop(lr=0.0001)),
                        b_f=250,
                        n_v=2),
                 list(m_n='5', 
                      n_d=0.3, 
                      n_u=600, 
                      n_l=0, 
                      l_f=list(name = 'msle', func = keras::loss_mean_squared_logarithmic_error),
                      o_f=list(name = 'rmsprop_0001', func = keras::optimizer_rmsprop(lr=0.0001)),
                      b_f=250,
                      n_v=2),
                   list(m_n='5', 
                        n_d=0.3, 
                        n_u=800, 
                        n_l=0, 
                        l_f=list(name = 'msle', func = keras::loss_mean_squared_logarithmic_error), 
                        o_f=list(name = 'rmsprop_0005', func = keras::optimizer_rmsprop(lr=0.0005)),
                        b_f=150,
                        n_v=100),
                   list(m_n='5', 
                        n_d=0.3, 
                        n_u=1000, 
                        n_l=0, 
                        l_f=list(name = 'msle', func = keras::loss_mean_squared_logarithmic_error), 
                        o_f=list(name = 'rmsprop_0005', func = keras::optimizer_rmsprop(lr=0.0005)),
                        b_f=150,
                        n_v=100))

#run models and save their metrics
for(mrl in run_list){
  hyper_results_list <- model_run(model_n_f = mrl$m_n,
            n_dropout_f = mrl$n_d,
            n_units_f = mrl$n_u,
            n_l_f = mrl$n_l,
            loss_func_f = mrl$l_f,
            optimizer_func_f = mrl$o_f,
            batch_size_f = mrl$b_f,
            num_vars_f = mrl$n_v)
  hyper_results <- hyper_results_list$hyper_results
  hyper_models <- hyper_results_list$hyper_models
}