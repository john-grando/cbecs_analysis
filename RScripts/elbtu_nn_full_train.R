#Train full neural network model

#Set up base and compile
source('RScripts/elbtu_nn_base_model.R')

train_reduced_df <- train_df %>% select(one_of(variables_by_importance[1:num_vars]))
test_reduced_df <- test_df %>% select(one_of(variables_by_importance[1:num_vars]))

#Select model parameters
model <- model_selector(model_n = '3', df = train_reduced_df, n_dropout=0.6, n_units=200, n_l = 0)
batch_size <- 150
optimizer_func <- keras::optimizer_rmsprop(lr=0.001)
loss_func <- keras::loss_mean_squared_logarithmic_error

#Compile
model %>% compile(
  #loss = 'mse',
  loss = loss_func,
  #loss = custom_loss_func,
  #optimizer = keras::optimizer_sgd(lr = 0.01, decay = 0.0),
  optimizer = optimizer_func,
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
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 200)

#Run
epochs <- 1000
history <- model %>% fit(
  as.matrix(train_reduced_df),
  as.matrix(train_labels),
  epochs = epochs,
  validation_data = list(as.matrix(test_reduced_df), as.matrix(test_labels)),
  batch_size = batch_size,
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
           object = model_name)

#save history
history_name <- 'ModelSaves/elbtu_nn_full_model_history.RData'
save(history, file = history_name)
put_object(file = history_name, 
           bucket = 'cuny-msds-final-project-cbecs', 
           object = history_name)