#Select model parameters
model <- model_selector(model_n = '5', df = train_reduced_df, n_dropout=0.3, n_units=1000, n_l = 0)
batch_size <- 150
optimizer_func <- keras::optimizer_rmsprop(lr=0.0005)
loss_func <- keras::loss_mean_squared_logarithmic_error
