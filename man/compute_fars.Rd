% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_fars.R
\name{compute_fars}
\alias{compute_fars}
\title{Compute Factor Augmented Quantile Regressions and Stressed Quantiles}
\usage{
compute_fars(
  dep_variable,
  factors,
  h = 1,
  edge = 0.05,
  scenario = NULL,
  min = TRUE,
  QTAU = 0.05
)
}
\arguments{
\item{dep_variable}{A numeric vector representing the dependent variable (e.g., GDP growth, inflation).}

\item{factors}{A matrix of factor estimates from a \code{mldfm} model.}

\item{h}{Integer. Forecast horizon (in time steps) for the quantile regression. Default is \code{1}.}

\item{edge}{Numeric. Trimming amount applied to the outermost quantiles (default \code{0.05}).}

\item{scenario}{Optional list of matrices representing a stressed scenario, as returned by \code{create_scenario()}.}

\item{min}{Logical. If \code{TRUE} (default), implement a stepwise minimization. If \code{FALSE}, implement a stepwise maximization.}

\item{QTAU}{Numeric. Quantile level (default \code{0.05}) used to compute stressed factors via \code{compute_stressed_factors()}. Only used if \code{scenario} is provided.}
}
\value{
A list containing:
\describe{
\item{\code{Quantiles}}{Matrix of forecasted quantiles (rows = time, cols = quantile levels).}
\item{\code{Strssed_Quantiles}}{Matrix of stressed scenario quantiles (same format), returned only if \code{scenario} is provided.}
\item{\code{Coeff}}{Matrix of quantile regression coefficients for each quantile.}
\item{\code{Std. Error}}{Matrix of Std. Error for each regression coefficient.}
\item{\code{Pvalue}}{Matrix of p-values for each regression coefficient.}
\item{\code{QTAU}}{The quantile level used to compute stressed factors (if applicable).}
\item{\code{Stressed_Factors}}{Matrix of selected stressed factors (only if \code{scenario} is provided and \code{QTAU} is set).}
}
}
\description{
Performs quantile regressions of a dependent variable on MLDFM-extracted factors.
Optionally generates quantile forecasts under stressed scenarios using hyperellipsoids.
}
\examples{
\donttest{
dep_variable <- rnorm(100)  # A numeric vector
data <- matrix(rnorm(100*300), nrow = 100, ncol = 300)
block_ind <- c(150, 300)  # Defines 2 blocks
global = 1
local <- c(1, 1)   
mldfm_result <- mldfm(data, blocks = 2, block_ind = block_ind, global = global , local = local)
fars_result <- compute_fars(dep_variable, mldfm_result$Factors, h = 1, edge = 0.05)
}
 
 
}
