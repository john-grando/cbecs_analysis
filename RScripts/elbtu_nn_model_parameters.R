#Select model parameters
model <- model_selector(model_n = '3', df = train_reduced_df, n_dropout=0.6, n_units=300, n_l = 0)
batch_size <- 150
optimizer_func <- keras::optimizer_rmsprop(lr=0.001)
loss_func <- keras::loss_mean_squared_logarithmic_error
