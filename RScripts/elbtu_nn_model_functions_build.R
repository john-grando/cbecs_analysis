#Various model functions and model selector

#Build 3h triangular rev 2 model
build_3h_tri_rev_2_model <- function(df = NA, n_dropout=0, n_units=10, n_l = 0) {
  model <- keras_model_sequential() %>%
    layer_dense(units =  floor(n_units / 3 / 3), 
                activation = "relu",
                input_shape = dim(df)[2],
                kernel_regularizer = regularizer_l2(l = n_l / 3)
    ) %>%
    layer_dropout(n_dropout / 3) %>%
    layer_dense(units = floor(n_units / 3), 
                activation = "relu",
                kernel_regularizer = regularizer_l2(l = n_l)
    ) %>%
    layer_dropout(n_dropout) %>%
    layer_dense(units = n_units, 
                activation = "relu",
                kernel_regularizer = regularizer_l2(l = n_l)
    ) %>%
    layer_dropout(n_dropout) %>%
    layer_dense(units = 1)
  
  model
  
}

#Build 3h rev triangular model
build_3h_tri_rev_model <- function(df = NA, n_dropout=0, n_units=10, n_l = 0) {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = floor(n_units / 3 / 3), 
                activation = "relu",
                input_shape = dim(df)[2],
                kernel_regularizer = regularizer_l2(l = n_l)
    ) %>%
    layer_dropout(n_dropout) %>%
    layer_dense(units = floor(n_units / 3), 
                activation = "relu",
                kernel_regularizer = regularizer_l2(l = n_l)
    ) %>%
    layer_dropout(n_dropout) %>%
    layer_dense(units = n_units, 
                activation = "relu",
                kernel_regularizer = regularizer_l2(l = n_l)
    ) %>%
    layer_dropout(n_dropout) %>%
    layer_dense(units = 1)
  
  model
  
}

#Build 3h triangular 2 model
build_3h_tri_2_model <- function(df = NA, n_dropout=0, n_units=10, n_l = 0) {
  model <- keras_model_sequential() %>%
    layer_dense(units = n_units, 
                activation = "relu",
                input_shape = dim(df)[2],
                kernel_regularizer = regularizer_l2(l = n_l)
    ) %>%
    layer_dropout(n_dropout) %>%
    layer_dense(units = floor(n_units / 3), 
                activation = "relu",
                kernel_regularizer = regularizer_l2(l = n_l)
    ) %>%
    layer_dropout(n_dropout / 3) %>%
    layer_dense(units = floor(n_units / 3 / 3), 
                activation = "relu",
                kernel_regularizer = regularizer_l2(l = n_l / 3)
    ) %>%
    layer_dropout(n_dropout) %>%
    layer_dense(units = 1)
  
  model
  
}

#Build 3h triangular model
build_3h_tri_model <- function(df = NA, n_dropout=0, n_units=10, n_l = 0) {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = n_units, 
                activation = "relu",
                input_shape = dim(df)[2],
                kernel_regularizer = regularizer_l2(l = n_l)
    ) %>%
    layer_dropout(n_dropout) %>%
    layer_dense(units = floor(n_units / 3), 
                activation = "relu",
                kernel_regularizer = regularizer_l2(l = n_l)
    ) %>%
    layer_dropout(n_dropout) %>%
    layer_dense(units = floor(n_units / 3 / 3), 
                activation = "relu",
                kernel_regularizer = regularizer_l2(l = n_l)
    ) %>%
    layer_dropout(n_dropout) %>%
    layer_dense(units = 1)
  
  model
  
}

#Build 3h model
build_3h_model <- function(df = NA, n_dropout=0, n_units=10, n_l = 0) {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = n_units, 
                activation = "relu",
                input_shape = dim(df)[2],
                kernel_regularizer = regularizer_l2(l = n_l)
    ) %>%
    layer_dropout(n_dropout) %>%
    layer_dense(units = n_units, 
                activation = "relu",
                kernel_regularizer = regularizer_l2(l = n_l)
    ) %>%
    layer_dropout(n_dropout) %>%
    layer_dense(units = n_units, 
                activation = "relu",
                kernel_regularizer = regularizer_l2(l = n_l)
    ) %>%
    layer_dropout(n_dropout) %>%
    layer_dense(units = 1)

  model
  
}

#build 2h model
build_2h_model <- function(df = NA, n_dropout=0, n_units=10, n_l = 0) {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = n_units, 
                activation = "relu",
                input_shape = dim(df)[2],
                kernel_regularizer = regularizer_l2(l = n_l)
    ) %>%
    layer_dropout(n_dropout) %>%
    layer_dense(units = n_units, 
                activation = "relu",
                input_shape = dim(df)[2],
                kernel_regularizer = regularizer_l2(l = n_l)
    ) %>%
    layer_dropout(n_dropout) %>%
    layer_dense(units = 1)
  
  model
}


#build 1h model
build_1h_model <- function(df = NA, n_dropout=0, n_units=10, n_l = 0) {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = n_units, 
                activation = "relu",
                input_shape = dim(df)[2],
                kernel_regularizer = regularizer_l2(l = n_l)
    ) %>%
    layer_dropout(n_dropout) %>%
    layer_dense(units = 1)
  
  model
}

#build 0h model
build_0h_model <- function(df = NA, n_dropout=0, n_units=10, n_l = 0) {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 1, 
                activation = "relu",
                input_shape = dim(df)[2],
                kernel_regularizer = regularizer_l2(l = n_l)
    )
  
  model
}

#model selector
model_selector <- function(model_n = '1', ...){
  model_n <- as.character(model_n)
  m_selected <- 0
  if(model_n=='0'){
    x <- build_0h_model(...)
    m_selected <- 1
  }
  if(model_n=='1'){
    x <- build_1h_model(...)
    m_selected <- 1
  }
  if(model_n=='2'){
    x <- build_2h_model(...)
    m_selected <- 1
  }
  if(model_n=='3'){
    x <- build_3h_model(...)
    m_selected <- 1
  }
  if(model_n=='4'){
    x <- build_3h_tri_model(...)
    m_selected <- 1
  }
  if(model_n=='5'){
    x <- build_3h_tri_2_model(...)
    m_selected <- 1
  }
  if(model_n=='6'){
    x <- build_3h_tri_rev_model(...)
    m_selected <- 1
  }
  if(model_n=='7'){
    x <- build_3h_tri_rev_2_model(...)
    m_selected <- 1
  }
  if(m_selected==0){stop('model does not exist')}
  return(x)
}