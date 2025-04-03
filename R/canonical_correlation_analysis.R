#' Canonical Correlation Analysis (for MLDFM)
#'
#' Computes factors using CCA across blocks, based on local PCA-extracted factors.
#'
#' @keywords internal
#' 
#' y: matrix containing all variables 
#' Nregio: vector containing the number of variables in each node
#' r_global: number of global factor to be extracted (final output)
#' r_reg: vector which specify the number of regional factors to be extracted in each node
#' 
canonical_correlation_analysis <- function(y, Nregio,r_glob,r_reg){
  
  
  
  g=length(Nregio) # number of node
  RegInd = c(0, cumsum(Nregio)) # cumulative sum of block size to determine start and end indices
  r = r_glob + r_reg # tot number of factors in each node (global + local) 
  rsum = cumsum(r) # cumulative number of factors
  
  # extract regional factors
  fi=c()
  
  # loop over each group
  for (i in 1:g){
    # compute eigenvectors
    evec<-eigen_sorted(t(y[,(RegInd[i]+1):RegInd[i+1]]) %*% y[,(RegInd[i]+1):RegInd[i+1]])$eigenvectors
    # extract r[i] largest eigen vectors (pca)
    # compute regional factors as f=y_region*evec
    # concatenate extracted factors into fi
    fi<-cbind(fi,y[,(RegInd[i]+1):RegInd[i+1]]%*%evec[, 1:r[i]])
  }
 
  # scale factors by their SD
  fi<-fi / kronecker(matrix(1,nrow = nrow(fi),ncol=1),t(matrix(sqrt(diag(t(fi)%*%fi)))))
  
  # extract global factors
  fhat=c()

  # loop over g-1 regions
  for (i in 1:(g-1)){
    # compute cross region local factors covariance (block1 vs later blocks at each iteration)
    C = t(fi[,1:rsum[1]]) %*% fi[,(rsum[i]+1):rsum[i+1]]
    # perform eigen-decomposition
    evec = eigen_sorted(t(C) %*% C)$eigenvectors
    # extract r_global principal components from the regional factors and store in fhat
    fhat = cbind(fhat,fi[,(rsum[i]+1):rsum[i+1]] %*% evec[,1:r_glob])
  }
  
  # compute covariance matrix of fhat factors
  C = t(fhat) %*% fhat 
  # perform pca 
  evec = eigen_sorted(t(C) %*% C)$eigenvectors
  # select final principal components as final global factors
  if((g-1)>1) fhatblock = fhat %*% evec[,1:r_glob] 
  if((g-1)==1) fhatblock = fhat %*% evec 

  return(fhatblock)
}