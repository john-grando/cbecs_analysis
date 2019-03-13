#Train full neural network model

#Set up base and compile
source('RScripts/elbtu_nn_base_model.R')
#Get model types
source('RScripts/elbtu_nn_model_functions_build.R')

#Select model
model <- model_selector(model_n = '3', df = train_df, n_dropout=0, n_units=200, n_l = 0)

#Compile
model %>% compile(
  loss = 'mse',
  #loss = keras::loss_mean_squared_logarithmic_error,
  #loss = custom_loss_func,
  #optimizer = keras::optimizer_sgd(lr = 0.01, decay = 0.0),
  optimizer = keras::optimizer_rmsprop(),
  metrics = list("mean_squared_error", "mean_squared_logarithmic_error")
)

#Function for model building
# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)

# The patience parameter is the amount of epochs to check for improvement.
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 50)

#Run
epochs <- 1000
history <- model %>% fit(
  as.matrix(train_df),
  as.matrix(train_labels),
  epochs = epochs,
  validation_data = list(as.matrix(test_df), as.matrix(test_labels)),
  batch_size = 50,
  verbose = 0,
  callbacks = list(print_dot_callback, 
                   early_stop
                   )
)

#save model
model_name <- 'ModelSaves/elbtu_nn_full_model.h5'
model %>% save_model_hdf5(model_name)
put_object(file = model_name, 
           bucket = 'cuny-msds-final-project-cbecs', 
           object = model_name, 
           multipart = TRUE)

#save history
history_name <- 'ModelSaves/elbtu_nn_full_model_history.RData'
save(history, file = history_name)
put_object(file = history_name, 
           bucket = 'cuny-msds-final-project-cbecs', 
           object = history_name, 
           multipart = TRUE)