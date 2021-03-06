% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/keras_models.R
\name{keras_cudnn_lstm}
\alias{keras_cudnn_lstm}
\title{keras_cudnn_lstm}
\usage{
keras_cudnn_lstm(input_dim, embed_dim = 128, seq_len = 50,
  filter_size = 5, n_filters = 100, pool_size = 4, lstm_dim = 64,
  bidirectional = F, dropout = 0.2, output_dim = 1,
  output_fun = "sigmoid")
}
\arguments{
\item{input_dim}{Number of unique vocabluary/tokens}

\item{embed_dim}{Number of word vectors}

\item{seq_len}{Length of the input sequences}

\item{lstm_dim}{Number of recurrent neurons (default 64)}

\item{output_dim}{Number of neurons of the output layer}

\item{output_fun}{Output activation function}

\item{lstm_drop}{Recurrent dropout ratio}
}
\value{
keras model
}
\description{
Word embedding + long short-term memory + GPU
}
