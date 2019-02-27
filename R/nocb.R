#' Next Observation Carried Backwards
#'
#' This function allows you to impute missing values with the next observation.
#' @param x Dataframe
#' @param rule What proportion of values need to be present before imputing? Defaults to 0.75
#' @keywords imputation
#' @export
#' @examples
#' nocb()

nocb <- function(x, rule=0.75) {
  for (row in 1:nrow(x)) {
    if (sum(is.na(x[row,]))/length(x[row,]) <= (1-rule)) {
      v <- !is.na(x[row,])
      x[row,] <- c(NA, x[row,][v])[cumsum(v)+as.numeric(!v)+1]
    }
  }
  return(x)
}
