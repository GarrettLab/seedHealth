
#' Generate truncated random normal variables where values outside range are assigned to range limit
#'
#' Note that this function is different from the one in the package truncnorm.
#'
#' Updated 2018-09-28

#' @param n the number of variables to be generated.
#' @param a the lower limit of range, positive numeric or positive numeric vector.
#' @param b the upper limit of range, positive numeric or positive numeric vector.
#' @param mean the mean of underlying normal distribution prior to truncation, positive numeric or positive numeric vector.
#' @param sd the standard deviation of underlying normal distribution prior to truncation, positive numeric or positive numeric vector.
#' @keywords misc
#' @importFrom stats median quantile rnorm var
#' @export
#' @examples
#' altruncnorm() # to be added



# for generating truncated normal random variables
altruncnorm <- function(n = 100, a = 0, b = 1, mean = 0, sd = 1){

  j <- rnorm(n,mean = mean, sd = sd)
  j[j < a] <- a
  j[j > b] <- b

  #---------------------------------------
  #Warning messages
  if (n <= 0 | floor(n) != n) {
    warning (paste('n must be a positive integer, your input is', n))
  } else if (a < 0 | a > 1) {
    warning (paste('a must be between 0 and 1, your input value is', a))
  } else if (b < 0 | b > 1) {
    warning (paste('b must be between 0 and 1, your input value is', b))
  } else if ( mean < 0 | mean > 1){
    warning (paste('mean must be between 0 and 1, your input value is', mean))
  } else if (sd < 0 | sd > 1) {
    warning (paste('sd must be between 0 and 1, your input value is', sd))
  } else{j}
  #----------------------------------------
}


