#' keras simple mlp
#'
#' Word Embedding + Simple Multilayer Perceptron 
#'
#' @param input_dim Number of unique vocabluary/tokens
#' @param embed_dim Number of word vectors
#' @param seq_len Length of the input sequences
#' @param embed_vectors word vectors
#' @param pooling one of c("flatten", "global_average", "average")
#' @param output_dim Number of neurons of the output layer
#' @param output_fun Output activation function
#' @return keras model
#' 
#' \if{html}{
#' \out{
#'  <img src = "https://media.giphy.com/media/IB9foBA4PVkKA/giphy.gif">
#' }}
#'
#' @export

keras_simple_mlp <- function(
  input_dim, embed_dim, seq_len, embed_vectors = NULL,
  pooling = 'flatten', 
  dense_dim = 128, dense_fun = 'relu', dropout = .5, 
  output_fun = 'softmax', output_dim = 1
){
  
  model <- keras::keras_model_sequential()
  
  if(is.null(embed_vectors)){
    #print("no vectors")
    model <- model %>%
      keras::layer_embedding(
        input_dim = input_dim,
        output_dim = embed_dim,
        input_length = seq_len
      )
  } else {
    #print("with vectors")
    model <- model %>% 
      layer_embedding(
        input_dim = input_dim,
        output_dim = embed_dim,
        weights = embed_vectors,
        input_length = seq_len,
        trainable = F
      )
  }
  
  # the 3D tensor of embeddings gets falttened into a 2D tensor of shape `(samples, maxlen * output_dim)
  if(pooling == 'global_average'){
    model <- model %>% keras::layer_global_average_pooling_1d() 
    #} else if(pooling == 'average'){
    # model <- model %>% keras::layer_average_pooling_1d(pool_size = ) 
  } else {
    model <- model %>% keras::layer_flatten()
  } 
  model <- model %>% 
    keras::layer_dense(units = dense_dim, activation = dense_fun) %>% 
    keras::layer_dropout(rate = dropout) %>% 
    keras::layer_dense(units = output_dim, activation = output_fun)
  
  return(model)
}


#' keras deep mlp
#'
#' Word Embedding + Deep Multilayer Perceptron 
#'
#' @param input_dim Number of unique vocabluary/tokens
#' @param embed_dim Number of word vectors
#' @param seq_len Length of the input sequences
#' @param hidden_dims Number of neurons per layer as vector of integers c(256, 128, 64)
#' @param hidden_fun Hidden activation function ("relu" by default)
#' @param output_dim Number of neurons of the output layer
#' @param output_fun Output activation function
#' @return keras model
#' 
#' @export

keras_deep_mlp <- function(
  input_dim, embed_dim = 64, seq_len, 
  hidden_dims = c(256, 128, 64), hidden_fun = "relu",
  output_fun = 'softmax',  output_dim = 1
){
  
  model <- keras::keras_model_sequential() %>% 
    keras::layer_embedding(input_dim = input_dim, output_dim = embed_dim, input_length = seq_len) %>%
    keras::layer_flatten()
  
  1:length(hidden_dims) %>% walk(~ model <- model %>% keras::layer_dense(units = hidden_dims[.x], activation = hidden_fun))
  
  model <- model %>% keras::layer_dense(units = output_dim, activation = output_fun)
  
  return(model)
}


#' keras lstm
#'
#' Word embedding + long short-term memory
#'
#' @param input_dim Number of unique vocabluary/tokens
#' @param embed_dim Number of word vectors
#' @param seq_len Length of the input sequences
#' @param lstm_dim Number of recurrent neurons (default 64)
#' @param lstm_drop Recurrent dropout ratio 
#' @param output_dim Number of neurons of the output layer
#' @param output_fun Output activation function
#' @return keras model
#' 
#' @export

keras_lstm <- function(
  input_dim, embed_dim = 128, seq_len = 50, 
  lstm_dim = 64, lstm_drop = .2, dropout = .2,
  output_dim = 1, output_fun = "softmax"
){
  
  model <- keras::keras_model_sequential() %>%
    keras::layer_embedding(
      input_dim = input_dim, 
      output_dim = embed_dim, 
      input_length = seq_len
    ) %>% 
    keras::layer_lstm(units = lstm_dim, dropout = dropout, recurrent_dropout = lstm_drop) %>%
    keras::layer_dense(units = output_dim, activation = output_fun)
  
  return(model)
}


#' keras_bi_lstm
#'
#' Word embedding + (bidirectional) long short-term memory
#'
#' @param input_dim Number of unique vocabluary/tokens
#' @param embed_dim Number of word vectors
#' @param seq_len Length of the input sequences
#' @param lstm_dim Number of recurrent neurons (default 64)
#' @param lstm_drop Recurrent dropout ratio 
#' @param output_dim Number of neurons of the output layer
#' @param output_fun Output activation function
#' @return keras model
#' 
#' @export

keras_bi_lstm <- function(
  input_dim, embed_dim = 128, seq_len = 50, 
  lstm_dim = 64, lstm_drop = .2, dropout = .2, 
  output_dim = 1, output_fun = "softmax"
){
  
  model <- keras::keras_model_sequential() %>%
    keras::layer_embedding(
      input_dim = input_dim, 
      output_dim = embed_dim, 
      input_length = seq_len
    ) %>% 
    keras::bidirectional(keras::layer_lstm(units = lstm_dim, dropout = dropout, recurrent_dropout = lstm_drop)) %>%
    keras::layer_dense(units = output_dim, activation = output_fun)

  return(model)
}

#' keras_deep_lstm
#'
#' Word embedding + Deep (bidirectional) long short-term memory
#' 
#' Stacking lstm modules of different size.
#'
#' @param input_dim Number of unique vocabluary/tokens
#' @param embed_dim Number of word vectors
#' @param seq_len Length of the input sequences
#' @param hidden_dims Number of neurons per layer as vector of integers c(256, 128, 64)
#' @param output_dim Number of neurons of the output layer
#' @param output_fun Output activation function
#' @return keras model
#' 
#' @export

keras_deep_lstm <- function(
  input_dim, embed_dim = 128, seq_len = 50, 
  hidden_dims = c(128, 64, 32),
  output_fun = "softmax", output_dim = 1
){
  
  model <- keras::keras_model_sequential() %>%
    keras::layer_embedding(
      input_dim = input_dim, 
      output_dim = embed_dim, 
      input_length = seq_len
    )
  
  # Dnymaically scale the network by increasing hidden_layer and hidden_dims 
  # for(layer in 1:length(hidden_dims))
  1:length(hidden_dims) %>% 
    walk(~{
      model <- model %>% 
        keras::layer_lstm(
          units =  hidden_dims[.x], 
          dropout = .2, 
          recurrent_dropout = .2, 
          return_sequences = T
        )
    })
  
  model <- model %>% 
    keras::layer_flatten() %>% 
    keras::layer_dense(units = output_dim, activation = output_fun)
  
  return(model)
}



#' keras_deep_bi_lstm
#'
#' Word embedding + Deep (bidirectional) long short-term memory
#' 
#' Stacking lstm modules of different size.
#'
#' @param input_dim Number of unique vocabluary/tokens
#' @param embed_dim Number of word vectors
#' @param seq_len Length of the input sequences
#' @param hidden_dims Number of neurons per layer as vector of integers c(256, 128, 64)
#' @param output_dim Number of neurons of the output layer
#' @param output_fun Output activation function
#' @return keras model
#' 
#' @export

keras_deep_bi_lstm <- function(
  input_dim, embed_dim = 128, seq_len = 50, 
  hidden_dims = c(128, 64, 32),
  output_fun = "softmax", output_dim = 1
){
  
  model <- keras::keras_model_sequential() %>%
    keras::layer_embedding(
      input_dim = input_dim, 
      output_dim = embed_dim, 
      input_length = seq_len
    )
  
  # Dnymaically scale the network by increasing hidden_layer and hidden_dims 
  # for(layer in 1:length(hidden_dims))
  1:length(hidden_dims) %>% 
    purrr::walk(~{
      model <- model %>% 
        keras::bidirectional(
          layer_lstm(
            units =  hidden_dims[.x], 
            dropout = .2, 
            recurrent_dropout = .2, 
            return_sequences = T
          )
        )
    })
  
  model <- model %>% 
    keras::layer_flatten() %>% 
    keras::layer_dense(units = output_dim, activation = output_fun)
  
  return(model)
}


#' keras cnn gru
#'
#' Word embedding + 1D pooled convolution + gru layer
#'
#' @param input_dim Number of unique vocabluary/tokens
#' @param embed_dim Number of word vectors
#' @param seq_len Length of the input sequences
#' @param n_filters the number of convolutional filters 
#' @param filter_size the window size (kernel_size)
#' @param pool_size pooling dimension (filters)
#' @param gru_dim Number of lstm neurons (default 32)
#' @param gru_drop default is 2
#' @param bidirectional default is F
#' @param output_dim Number of neurons of the output layer
#' @param output_fun Output activation function
#' @return keras model
#' 
#' @export

keras_cnn_gru <- function(
  input_dim, embed_dim = 128, seq_len = 50, 
  filter_size = 5, n_filters = 100, pool_size = 4, 
  gru_dim = 64, gru_drop = .2, bidirectional = F,
  output_dim = 1, output_fun = "sigmoid"
){
  
  filter_size <- embed_dim/2
  model <- keras::keras_model_sequential() %>%
    keras::layer_embedding(
      input_dim = input_dim, 
      output_dim = embed_dim, 
      input_length = seq_len
    ) %>%
    #layer_dropout(0.25) %>%
    keras::layer_conv_1d(
      n_filters, 
      filter_size, # -> kernel_size
      padding = "valid",
      activation = "relu",
      strides = 1
    ) %>%
    keras::layer_max_pooling_1d(pool_size)
  
  if(bidirectional){
    model <- model %>% keras::bidirectional(
      keras::layer_gru(units = gru_dim, dropout = .2, recurrent_dropout = gru_drop)
    )
  } else {
    model <- model %>% keras::layer_gru(units = gru_dim, dropout = .2, recurrent_dropout = gru_drop)
  }
  
  model <- model %>%
    keras::layer_dense(units = output_dim, activation = output_fun) 
  
  return(model)
}


#' keras cnn lstm
#'
#' Word embedding + 1D pooled convolution + lstm layer
#'
#' @param input_dim Number of unique vocabluary/tokens
#' @param embed_dim Number of word vectors
#' @param seq_len Length of the input sequences
#' @param n_filters the number of convolutional filters 
#' @param filter_size the window size (kernel_size)
#' @param pool_size pooling dimension (filters)
#' @param lstm_dim Number of lstm neurons (default 32)
#' @param lstm_drop default is 2
#' @param bidirectional default is F
#' @param output_dim Number of neurons of the output layer
#' @param output_fun Output activation function
#' @return keras model
#' 
#' @export

keras_cnn_lstm <- function(
  input_dim, embed_dim = 128, seq_len = 50, 
  filter_size = 5, n_filters = 100, pool_size = 4, 
  lstm_dim = 64, lstm_drop = .2, bidirectional = F, dropout = .2, 
  output_dim = 1, output_fun = "sigmoid"
){
  
  model <- keras::keras_model_sequential() %>%
    keras::layer_embedding(
      input_dim = input_dim, 
      output_dim = embed_dim, 
      input_length = seq_len
    ) %>%
    #layer_dropout(0.25) %>%
    keras::layer_conv_1d(
      n_filters, 
      filter_size, # -> kernel_size
      padding = "valid",
      activation = "relu",
      strides = 1
    ) %>%
    keras::layer_max_pooling_1d(pool_size)
  
  if(bidirectional){
    model <- model %>% keras::bidirectional(
      keras::layer_lstm(units = lstm_dim, dropout = dropout, recurrent_dropout = lstm_drop)
    )
  } else {
    model <- model %>% keras::layer_lstm(units = lstm_dim, dropout = dropout, recurrent_dropout = lstm_drop)
  }
  
  model <- model %>% keras::layer_dense(units = output_dim, activation = output_fun) 
  
  return(model)
}


#' keras gru cnn
#'
#' Word embedding + gru global average & max + 1D pooled convolution 
#'
#' @param input_dim Number of unique vocabluary/tokens
#' @param embed_dim Number of word vectors
#' @param seq_len Length of the input sequences
#' @param gru_dim Number of lstm neurons (default 32)
#' @param gru_drop default is 2
#' @param n_filters the number of convolutional filters 
#' @param filter_size the window size (kernel_size)
#' @param pool_size pooling dimension (filters)
#' @param output_dim Number of neurons of the output layer
#' @param output_fun Output activation function
#' @return keras model
#' 
#' @export

keras_gru_cnn <- function(
  input_dim, embed_dim = 128, seq_len = 50, 
  gru_dim = 64, gru_drop = .2, #bidirectional = T,
  filter_sizes = c(3, 2), n_filters = c(120, 60), pool_size = 4, 
  output_fun = "sigmoid", output_dim = 1
){
  
  input <- keras::layer_input(shape = seq_len, dtype = "int32", name = "input")
  
  embedding <- input %>%
    keras::layer_embedding(
      input_dim = input_dim, 
      output_dim = embed_dim
      #input_length = seq_len
    ) %>%
    keras::layer_spatial_dropout_1d(rate = .1)
  
  block1 <- embedding %>%
    keras::bidirectional(keras::layer_gru(units = gru_dim, return_sequences = T, recurrent_dropout = gru_drop)) %>% 
    keras::layer_conv_1d(n_filters[1], filter_sizes[1], padding = "valid", activation = "relu", strides = 1) 
  
  block2 <- embedding %>%
    keras::bidirectional(keras::layer_gru(units = gru_dim, return_sequences = T, recurrent_dropout = gru_drop)) %>% 
    keras::layer_conv_1d(n_filters[2], filter_sizes[2], padding = "valid", activation = "relu", strides = 1) 
  
  max_pool1 <- block1 %>% keras::layer_global_max_pooling_1d()
  ave_pool1 <- block1 %>% keras::layer_global_average_pooling_1d()
  max_pool2 <- block2 %>% keras::layer_global_max_pooling_1d()
  ave_pool2 <- block2 %>% keras::layer_global_average_pooling_1d()
  
  output <- keras::layer_concatenate(list(ave_pool1, max_pool1, ave_pool2, max_pool2)) %>%
    keras::layer_dense(units = output_dim, activation = output_fun)
  
  model <- keras::keras_model(input, output)
  
  return(model)
}

#' keras_gru
#'
#' Word embedding + spatial dropout + (pooled) gated recurrent unit
#' 
#' Taken from https://www.kaggle.com/yekenot/pooled-gru-fasttext
#'
#' @param input_dim Number of unique vocabluary/tokens
#' @param embed_dim Number of word vectors
#' @param seq_len Length of the input sequences
#' @param gru_dim Number of recurrent neurons (default 64)
#' @param gru_drop Recurrent dropout ratio 
#' @param output_dim Number of neurons of the output layer
#' @param output_fun Output activation function
#' @return keras model
#' 
#' @export

keras_gru <- function(
  input_dim, embed_dim = 128, seq_len = 50, 
  gru_dim = 64, gru_drop = .2, 
  output_fun = "sigmoid", output_dim = 1
){
  
  input <- keras::layer_input(shape = seq_len)
  
  block <- input %>%
    keras::layer_embedding(
      input_dim = input_dim, 
      output_dim = embed_dim 
      #input_length = maxlen
    ) %>% 
    keras::layer_spatial_dropout_1d(0.2) %>% 
    keras::layer_gru(units = gru_dim, return_sequences = T)
  
  ### global average
  avg_pool <- block %>% keras::layer_global_average_pooling_1d()
  ### global max
  max_pool <- block %>% keras::layer_global_max_pooling_1d()
  
  output <- keras::layer_concatenate(c(avg_pool, max_pool)) %>% 
    keras::layer_dense(output_dim, activation = output_fun)
  
  model <- keras::keras_model(input, output)
  
  return(model)
}



#' keras_bi_gru
#'
#' Word embedding + spatial dropout + (pooled) gated recurrent unit
#' 
#' Taken from https://www.kaggle.com/yekenot/pooled-gru-fasttext
#'
#' @param input_dim Number of unique vocabluary/tokens
#' @param embed_dim Number of word vectors
#' @param seq_len Length of the input sequences
#' @param gru_dim Number of recurrent neurons (default 64)
#' @param gru_drop Recurrent dropout ratio 
#' @param output_dim Number of neurons of the output layer
#' @param output_fun Output activation function
#' @return keras model
#' 
#' @export

keras_bi_gru <- function(
  input_dim, embed_dim = 128, seq_len = 50, 
  gru_dim = 64, gru_drop = .2, bidirectional = F,
  output_fun = "sigmoid", output_dim = 1
){
  
  input <- keras::layer_input(shape = seq_len)
  
  block <- input %>%
    keras::layer_embedding(
      input_dim = input_dim, 
      output_dim = embed_dim 
      #input_length = maxlen
    ) %>% 
    keras::layer_spatial_dropout_1d(0.2) %>% 
    keras::bidirectional(keras::layer_gru(units = gru_dim, return_sequences = T))

  ### global average
  avg_pool <- block %>% keras::layer_global_average_pooling_1d()
  ### global max
  max_pool <- block %>% keras::layer_global_max_pooling_1d()
  
  output <- keras::layer_concatenate(c(avg_pool, max_pool)) %>% 
    keras::layer_dense(output_dim, activation = output_fun)
  
  model <- keras::keras_model(input, output)
  
  return(model)
}


#' keras_multi_cnn
#'
#' @param input_dim Number of unique vocabluary/tokens
#' @param embed_dim Number of word vectors
#' @param seq_len Length of the input sequences
#' @param n_filters the number of convolutional filters 
#' @param filter_size the window size (kernel_size)
#' @param output_dim Number of neurons of the output layer
#' @param output_fun Output activation function
#' @return keras model
#' 
#' @export

keras_multi_cnn <- function(
  input_dim, embed_dim = 128, seq_len = 50, 
  filter_sizes = c(1, 2, 3, 4), n_filters = 50,
  output_dim = 1, output_fun = "sigmoid"
){
  
  inputs <- keras::layer_input(shape = seq_len)
  
  embedding<- inputs %>%
    keras::layer_embedding(
      input_dim = input_dim, 
      output_dim = embed_dim
      #input_length = seq_len
    ) %>% 
    #layer_spatial_dropout_1d(0.2) %>% 
    keras::layer_reshape(list(seq_len, embed_dim, 1))
  
  block1 <- embedding %>% 
    keras::layer_conv_2d(
      n_filters, 
      kernel_size = list(filter_sizes[1], embed_dim), 
      kernel_initializer = 'normal',
      activation='elu'
    ) %>% 
    keras::layer_max_pooling_2d(pool_size=list(seq_len - filter_sizes[1] + 1, 1))
  
  block2 <- embedding %>% 
    keras::layer_conv_2d(
      n_filters, 
      kernel_size = list(filter_sizes[2], embed_dim), 
      kernel_initializer = 'normal',
      activation='elu'
    ) %>% 
    keras::layer_max_pooling_2d(pool_size=list(seq_len - filter_sizes[2] + 1, 1))
  
  block3 <- embedding %>% 
    keras::layer_conv_2d(
      n_filters, 
      kernel_size = list(filter_sizes[3], embed_dim), 
      kernel_initializer = 'normal',
      activation='elu'
    ) %>% 
    keras::layer_max_pooling_2d(pool_size = list(seq_len - filter_sizes[3] + 1, 1))
  
  block4 <- embedding %>% 
    keras::layer_conv_2d(
      n_filters, 
      kernel_size = list(filter_sizes[4], embed_dim), 
      kernel_initializer = 'normal',
      activation='elu'
    ) %>% 
    keras::layer_max_pooling_2d(pool_size=list(seq_len - filter_sizes[4] + 1, 1))
  
  z <- keras::layer_concatenate(list(block1, block2, block3, block4), axis = 1) %>% 
    keras::layer_flatten()
  # does not work quite well
  #keras::layer_dropout(dropout)
  
  output <- z %>% 
    keras::layer_dense(output_dim, activation=output_fun)
  
  model <- keras::keras_model(inputs, output)
  
  return(model)
}


#' keras_sep_cnn
#'
#' @param input_dim Number of unique vocabluary/tokens
#' @param embed_dim Number of word vectors
#' @param seq_len Length of the input sequences
#' @param n_filters the number of convolutional filters 
#' @param filter_size the window size (kernel_size)
#' @param output_dim Number of neurons of the output layer
#' @param output_fun Output activation function
#' @return keras model
#' 
#' @export
keras_sep_cnn <- function(
  input_dim, embed_dim, seq_len, embed_vectors = NULL,
  n_filters = 30,
  output_fun = 'sigmoid', output_dim = 1
){
  
  model <- keras::keras_model_sequential()
  
  if(is.null(embed_vectors)){
    #print("no vectors")
    model <- model %>%
      keras::layer_embedding(
        input_dim = input_dim,
        output_dim = embed_dim,
        input_length = seq_len
      )
  } else {
    #print("with vectors")
    model <- model %>% 
      layer_embedding(
        input_dim = input_dim,
        output_dim = embed_dim,
        weights = embed_vectors,
        input_length = seq_len,
        trainable = F
      )
  }
  
  model <- model %>% 
    keras::layer_dropout(.2) %>% 
    keras::layer_separable_conv_1d(filters = n_filters, kernel_size = 3, activation = "relu") %>% 
    #keras::layer_separable_conv_1d(filters = n_filters, kernel_size = 3, activation = "relu") %>% 
    keras::layer_max_pooling_1d(pool_size = 1) %>%
    keras::layer_dense(units = output_dim, activation = output_fun)
  
  # # the 3D tensor of embeddings gets falttened into a 2D tensor of shape `(samples, maxlen * output_dim)
  # if(pooling == 'global_average'){
  #   model <- model %>% keras::layer_global_average_pooling_1d() 
  #   #} else if(pooling == 'average'){
  #   # model <- model %>% keras::layer_average_pooling_1d(pool_size = ) 
  # } else {
  #   model <- model %>% keras::layer_flatten()
  # } 
  
  return(model)
}



#' keras_cudnn_lstm 
#'
#' Word embedding + long short-term memory + GPU
#'
#' @param input_dim Number of unique vocabluary/tokens
#' @param embed_dim Number of word vectors
#' @param seq_len Length of the input sequences
#' @param lstm_dim Number of recurrent neurons (default 64)
#' @param lstm_drop Recurrent dropout ratio 
#' @param output_dim Number of neurons of the output layer
#' @param output_fun Output activation function
#' @return keras model
#' 
#' @export

keras_cudnn_lstm <- function(
  input_dim, embed_dim = 128, seq_len = 50, filter_size = 5, 
  n_filters = 100, pool_size = 4, lstm_dim = 64, #lstm_drop = 0.2, 
  bidirectional = F, dropout = 0.2, output_dim = 1, output_fun = "sigmoid"
){
  
  model <- keras::keras_model_sequential() %>% 
    keras::layer_embedding(
      input_dim = input_dim, 
      output_dim = embed_dim, 
      input_length = seq_len
    )
  
  if (bidirectional) {
    model <- model %>% keras::bidirectional(keras::layer_cudnn_lstm(units = lstm_dim))
  }
  else {
    model <- model %>% keras::layer_cudnn_lstm(units = lstm_dim)
  }
  model <- model %>% keras::layer_dense(units = output_dim, activation = output_fun)
  return(model)
}

#' keras_cudnn_cnn_lstm
#'
#' Word embedding + 1D pooled convolution + lstm layer + GPU
#'
#' @param input_dim Number of unique vocabluary/tokens
#' @param embed_dim Number of word vectors
#' @param seq_len Length of the input sequences
#' @param n_filters the number of convolutional filters 
#' @param filter_size the window size (kernel_size)
#' @param pool_size pooling dimension (filters)
#' @param lstm_dim Number of lstm neurons (default 32)
#' @param lstm_drop default is 2
#' @param bidirectional default is F
#' @param output_dim Number of neurons of the output layer
#' @param output_fun Output activation function
#' @return keras model
#' 
#' @export

keras_cudnn_cnn_lstm <- function (
  input_dim, embed_dim = 128, seq_len = 50, filter_size = 5,
  n_filters = 100, pool_size = 4, lstm_dim = 64, #lstm_drop = 0.2,
  bidirectional = F, dropout = 0.2, output_dim = 1, output_fun = "sigmoid"
  ){
  
  model <- keras::keras_model_sequential() %>%
    keras::layer_embedding(
      input_dim = input_dim,
      output_dim = embed_dim, 
      input_length = seq_len
    ) %>%
    keras::layer_conv_1d(
      n_filters,
      filter_size, 
      padding = "valid", 
      activation = "relu",
      strides = 1
    ) %>%
    keras::layer_max_pooling_1d(pool_size)
  
  if (bidirectional) {
    model <- model %>% keras::bidirectional(keras::layer_cudnn_lstm(units = lstm_dim))
  }
  else {
    model <- model %>% keras::layer_cudnn_lstm(units = lstm_dim)
  }
  model <- model %>% keras::layer_dense(units = output_dim, activation = output_fun)
  return(model)
}
