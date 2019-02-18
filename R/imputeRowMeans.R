#' Row Means Imputation
#'
#' This function allows you to impute missing values with the row mean. 
#' @param x Dataframe
#' @param rule What proportion of values need to be present before imputing? Defaults to 0.75
#' @keywords imputation
#' @export
#' @examples
#' imputeRowMeans()

imputeRowMeans <- function(x, rule=0.75) {
  for (row in 1:nrow(x)) {
    if (sum(is.na(x[row,]))/length(x[row,]) <= (1-rule)) {
      x[row,][is.na(x[row,])] <- mean(as.numeric(x[row,]), na.rm=TRUE)
    }
  }
  return(x)
}