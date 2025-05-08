#' Correct Outliers in a Dataset
#'
#' This function identifies and corrects outliers in a dataset using principal 
#' component analysis (PCA). It scales the data, performs PCA, computes idiosyncratic 
#' components, and replaces values that fall outside a defined outlier threshold with 
#' the median of 5 previous values. The outlier threshold is determined using the 
#' interquartile range (IQR) method.
#'
#' @param data A numeric matrix or data frame where rows represent observations and 
#'        columns represent variables.
#' @param r An integer specifying the number of principal components to use for PCA.
#'
#' @return A list containing:
#' \item{data}{A matrix with corrected data where outliers are replaced by the median of previous values.}
#' \item{outliers}{A binary matrix (same dimensions as the input data) indicating the position of outliers.}
#'
#'
#' @examples
#' data <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' result <- correct_outliers(data, r = 3)
#' corrected_data <- result$data
#' outliers_matrix <- result$outliers
#'
#'@importFrom stats IQR
#'
#' @export
correct_outliers <- function(data, r ) {
  
  X<-scale(data,TRUE,TRUE)
  
  
  t<-nrow(X)
  N<-ncol(X)
 
  N_median = 5
  
  eR<-eigen(X%*%t(X))
  values<-eR$values[c(1:r)]
  vectors<-matrix(eR$vectors[,c(1:r)],t,r)
  
  F_hat<-sqrt(t)*vectors
  P_hat<-(1/t)*t(F_hat)%*%X
  
  # Compute idiosyncratic components
  Idio_comp<-X-F_hat%*%P_hat
  
  # Search and correct outliers
  outliers <- matrix(0,t,N)
  
  for (i in 1:N) {
    max <- quantile(Idio_comp[,i],0.75) + (IQR(Idio_comp[,i]) * 6)
    min <- quantile(Idio_comp[,i],0.25) - (IQR(Idio_comp[,i]) * 6)
    for (ii in (N_median+1):t) {
      if (Idio_comp[ii,i] < min | Idio_comp[ii,i] > max) {
        outliers[ii,i]<-1 
        #cat("Replacing value at", ii, i, "with median\n")
        data[ii,i]<-median(data[c((ii-N_median):(ii-1)),i])} 
    }
  }
  
  return(list(data = data, outliers = outliers))
}

