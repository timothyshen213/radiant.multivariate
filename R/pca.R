#' Principal Component Analysis
#'
#' @details Principal Component Analysis ....
#'
#' @param dataset Dataset
#' @param pca_scale Boolean value if the dataset is to be scaled
#' @param pca_center Boolean value if the dataset is to be centered
#' @param pca_pc Maximum principal components (default is equal to the number of variables)
#' @param pca_plot Boolean value if the PCA is to be visualized
#'
#' @return A list of all variables used in pca as an object of class pca
#'
#'
#' @import dplyr
#'
#' @export
pca <- function(dataset, pca_scale, pca_center, pca_pc){
  df<-dataset %>% select(where(is.numeric)) # Extracts only numerical variables from the dataset
  if (ncol(df)<=0){
    return("There are no numerical variables in the dataset. It is not suggested to use Principal Components Analysis for non-numerical data.")
  }
  pca_pc = as.numeric(pca_pc)
  if (pca_pc > ncol(df)){
    return("The number of principal components exceed the number of numerical variables from the dataset")
  }
  df_prcomp<-prcomp(df,center=pca_center,scale.=pca_scale,rank.=pca_pc)
}

#' Summary method for the pca function
#'
#' @details Principal Component Analysis ....
#'
#' @param object Return value from \code{\link{pca}}
#' @param ... further arguments passed to or from other methods
#'
#' @import dplyr
#'
#' @export
summary.pca <- function(object,...){
  cat("Principal Component Analysis \n")
  pca_display<-object$df_prcomp
  pca_display %<>% print()
  cat("\n\n")
  cat("Standard Deviations of the Principal Components")
  sd_display<-object$df_prcomp$sdev
  sd_display %<>% print()
}
