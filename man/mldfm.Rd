% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mldfm.R
\name{mldfm}
\alias{mldfm}
\title{Estimate Multilevel Dynamic Factor Model}
\usage{
mldfm(
  data,
  blocks = 1,
  block_ind = NULL,
  global = 1,
  local = NULL,
  middle_layer = NULL,
  method = 0,
  tol = 1e-06,
  max_iter = 1000,
  verbose = TRUE
)
}
\arguments{
\item{data}{A numeric matrix or data frame containing the time series data. Rows represent time points; columns represent observed variables.}

\item{blocks}{Integer. Number of blocks into which the data is divided.}

\item{block_ind}{Integer vector. End column indices for each block. Must be of length \code{blocks} and in increasing order.}

\item{global}{Integer. Number of global factors extracted from the entire dataset.}

\item{local}{Integer vector of length \code{blocks}. Specifies the number of local factors for each block.}

\item{middle_layer}{Named list. Each name is a string specifying a group of blocks (e.g., \code{"1-3"} or \code{"2-3"}), and each value is the number of factors to extract.}

\item{method}{Integer.  Method used to initialize the factors: \code{0} for Canonical Correlation Analysis (CCA), \code{1} for Principal Component Analysis (PCA).}

\item{tol}{Numeric. The tolerance level for the residual sum of squares (RSS) minimization process. Used as a convergence criterion.}

\item{max_iter}{Integer. The maximum number of iterations allowed for the RSS minimization process.}

\item{verbose}{Logical. If \code{TRUE} (default), print a summary of the mldfm.}
}
\value{
An object of class \code{mldfm}, which is a list containing the following components:
\describe{
\item{Factors}{Matrix of estimated factors.}
\item{Lambda}{Matrix of factor loadings.}
\item{Residuals}{Matrix of residuals.}
\item{Iterations}{Number of iterations before convergence.}
\item{Factors_list}{List of estimated factors for each node.}
}
}
\description{
Estimates a multilevel dynamic factor model from time series data.
Supports both single-block and hierarchical multi-block structures with customizable factor extraction settings.
}
\examples{
\donttest{
data <- matrix(rnorm(1000), nrow = 100, ncol = 519)
block_ind <- c(63, 311, 519)  # Defines 3 blocks
local <- c(1, 1, 1)           # One local factor per block
middle_layer <- list("1-3" = 1)
result <- mldfm(data, blocks = 3, block_ind = block_ind, global = 1, 
local = local, middle_layer = middle_layer)
summary(result)
}

}
