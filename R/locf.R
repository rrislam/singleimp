#' Last Observation Carried Forward
#'
#' This function allows you to impute missing values with the previous observation. 
#' @param x Dataframe
#' @param rule What proportion of values need to be present before imputing? Defaults to 0.75
#' @keywords imputation
#' @export
#' @examples
#' locf()

locf <- function(x, rule=0.75) {
  for (row in 1:nrow(x)) {
    if (sum(is.na(x[row,]))/length(x[row,]) <= (1-rule)) {
      v <- !is.na(x[row,])
      x[row,] <- c(NA, x[row,][v])[cumsum(v)+1]
    }
  }
  return(x)
}