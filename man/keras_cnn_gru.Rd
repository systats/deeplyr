% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/keras_models.R
\name{keras_cnn_gru}
\alias{keras_cnn_gru}
\title{keras cnn gru}
\usage{
keras_cnn_gru(input_dim, embed_dim = 128, seq_len = 50,
  filter_size = 5, n_filters = 100, pool_size = 4, gru_dim = 64,
  gru_drop = 0.2, bidirectional = F, output_dim = 1,
  output_fun = "sigmoid")
}
\arguments{
\item{input_dim}{Number of unique vocabluary/tokens}

\item{embed_dim}{Number of word vectors}

\item{seq_len}{Length of the input sequences}

\item{filter_size}{the window size (kernel_size)}

\item{n_filters}{the number of convolutional filters}

\item{pool_size}{pooling dimension (filters)}

\item{gru_dim}{Number of lstm neurons (default 32)}

\item{gru_drop}{default is 2}

\item{bidirectional}{default is F}

\item{output_dim}{Number of neurons of the output layer}

\item{output_fun}{Output activation function}
}
\value{
keras model
}
\description{
Word embedding + 1D pooled convolution + gru layer
}
