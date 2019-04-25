#Select model parameters
model <- model_selector(model_n = '5', df = train_reduced_df, n_dropout=0.3, n_units=600, n_l = 0)
batch_size <- 350
optimizer_func <- keras::optimizer_rmsprop(lr=0.0001)
loss_func <- custom_loss_func
  #keras::loss_mean_squared_logarithmic_error
