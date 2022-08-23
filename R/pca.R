#' Principal Component Analysis
#'
#' @details Principal Component Analysis ....
#'
#' @param dataset Dataset
#' @param vars Vector of variables to include in the analysis
#' @param scal Boolean value if the dataset is to be scaled
#' @param cent Boolean value if the dataset is to be centered
#' @param pc Maximum principal components (default is equal to the number of variables)
#' @param plotpc Boolean value if the PCA is to be visualized
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param envir Environment to extract data from
#'
#' @return A list of all variables used in pca as an object of class pca
#'
#'
#' @import dplyr
#'
#' @export
pca <- function(dataset, vars, scal, cent, pc, data_filter = "",
                envir = parent.frame()){
  labels = "none"
  if (is_not(pc)) pc <- 2
  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, if (labels == "none") vars else c(labels, vars), filt = data_filter, envir = envir) %>%
    as.data.frame() %>%
    mutate_if(is.Date, as.numeric)
  rm(envir)
  anyCategorical <- sapply(dataset, function(x) is.numeric(x)) == FALSE
  ## in case : is used
  if (length(vars) < ncol(dataset)) vars <- colnames(dataset)
  if (labels != "none") {
    if (length(unique(dataset[[1]])) == nrow(dataset)) {
      rownames(dataset) <- dataset[[1]]
    } else {
      message("\nThe provided labels are not unique. Please select another labels variable\n")
      rownames(dataset) <- seq_len(nrow(dataset))
    }
    dataset <- select(dataset, -1)
  }
  if (ncol(dataset)<=0){
    return("There are no numerical variables in the dataset. It is not suggested to use Principal Components Analysis for non-numerical data." %>%
             add_class("pca"))
  }
  pc = as.numeric(pc)
  if (pc > ncol(dataset)){
    return("The number of principal components exceed the number of numerical variables." %>%
             add_class("pca"))
  }
  df_prcomp<-prcomp(dataset,center=cent,scale.=scal,rank.=pc)
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
}

#' Plot method for the pca function
#'
#' @details Plotting a PCA ...
#'
#' @param x Return value from \code{\link{pca}}
#' @param plots Plots to return.
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{hclus}} to generate results
#' @seealso \code{\link{summary.hclus}} to summarize results
#'
#' @export
plot.pca <- function(x,plots = c("scree", "biplot"),shiny = FALSE, custom = FALSE, ...){
  if (radiant.data::is_empty(plots)) return(invisible())
  if (is.character(x)) return(invisible())
  var_explained = (x$df_prcomp$sdev^2)/sum(x$df_prcomp$sdev^2)
  plot_list <- list()
  plot_list[[1]] <-
      qplot(c(1:x$pc), var_explained) + geom_line() + xlab("Principal Component")+ylab("Variance Explained")+ ggtitle("Scree Plot") + ylim(0,1)
  plot_list[[2]] <-
      biplot(x$df_prcomp, xlabs="Principal Component 1", ylabs="Principal Component 2", main="Biplot")

  if (length(plot_list) > 0) {
    if (custom) {
      if (length(plot_list) == 1) plot_list[[1]] else plot_list
    } else {
      patchwork::wrap_plots(plot_list, ncol = 1) %>%
        {if (shiny) . else print(.)}
    }
  }
}
