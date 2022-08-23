#' #' Principal Component Anlaysis
#' #'
#' #' @details See \url{https://radiant-rstats.github.io/docs/multivariate/hclus.html} for an example in Radiant
#' #'
#' #' @param dataset Dataset
#' #' @param pca_scale Boolean value if the dataset is to be scaled
#' #' @param pca_center Boolean value if the dataset is to be centered
#' #' @param pca_pc Maximum principal components (default is equal to the number of variables)
#' #'
#' #' @return A list of all variables used in pca as an object of class pca
#' #'
#' #' @examples
#' #' hclus(shopping, vars = "v1:v6") %>% str()
#' #'
#' #' @seealso \code{\link{summary.hclus}} to summarize results
#' #' @seealso \code{\link{plot.hclus}} to plot results
#' #'
#' #' @importFrom gower gower_dist
#' #'
#' #' @export
#' pca <- function(dataset, pca_scale, pca_center, pca_pc){
#'   df<-dataset %>% select(where(is.numeric)) # Extracts only numerical variables from the dataset
#'   if (ncol(df)<=0){
#'     return("There are no numerical variables in the dataset. It is not suggested to use Principal Components Analysis for non-numerical data.")
#'   }
#'   pca_pc = as.numeric(pca_pc)
#'   if (pca_pc > ncol(df)){
#'     return("The number of principal components exceed the number of numerical variables from the dataset")
#'   }
#'   df_prcomp<-prcomp(df,center=pca_center,scale.=pca_scale,rank.=pca_pc)
#' }
#'
#' #' Summary method for the pca function
#' #'
#' #' @details See \url{https://radiant-rstats.github.io/docs/multivariate/hclus.html} for an example in Radiant
#' #'
#' #' @param object Return value from \code{\link{pca}}
#' #' @param ... further arguments passed to or from other methods
#' #'
#' #' @examples
#' #' result <- hclus(shopping, vars = c("v1:v6"))
#' #' summary(result)
#' #'
#' #' @seealso \code{\link{hclus}} to generate results
#' #' @seealso \code{\link{plot.hclus}} to plot results
#' #'
#' #' @export
#' summary.pca <- function(object,...){
#'   cat("Principal Component Analysis \n")
#'   pca_display<-object$df_prcomp
#'   pca_display %<>% print()
#'   cat("\n\n")
#'   cat("Standard Deviations of the Principal Components")
#'   sd_display<-object$df_prcomp$sdev
#'   sd_display %<>% print()
#' }
#'
#' #' Plot method for the pca function
#' #'
#' #' @details See \url{https://radiant-rstats.github.io/docs/multivariate/hclus.html} for an example in Radiant
#' #'
#' #' @param object Return value from \code{\link{pca}}
#' #' @param ... further arguments passed to or from other methods
#' #'
#' #' @examples
#' #' result <- hclus(shopping, vars = c("v1:v6"))
#' #' summary(result)
#' #'
#' #' @seealso \code{\link{hclus}} to generate results
#' #' @seealso \code{\link{plot.hclus}} to plot results
#' #'
#' #' @export
