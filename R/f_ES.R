#' Expected Shortfall
#'
#' @param alpha Numeric argument for Expected-Shortfall, between 0 and 1
#' @param type_function String argument : "gaussian" for normal distribution or "skew-t" for t-student distribution
#' @param params Numeric vector containing parameters of the distribution
#' @param accuracy Scalar value which regulates the accuracy of the ES (default value 1e-05)
#'
#' @description
#' The function allows to calculate Expected-shortfall for a given distribution. It takes as parameters alpha (risk level), a distribution and the parameters associated with this distribution. For example, for a normal distribution, the user must enter the mean and the standard deviation. Currently, the function can calculate the Expected-shortfall for the normal distribution and for the skew-t distribution (Azzalini and Capitianio, 2003)
#'
#' @references Azzalini, Adelchi, and Antonella Capitanio. "Distributions generated by perturbation of symmetry with emphasis on a multivariate skew t‐distribution." Journal of the Royal Statistical Society: Series B (Statistical Methodology) 65.2 (2003): 367-389.
#' @references Azzalini, Adelchi, and Maintainer Adelchi Azzalini. "Package ‘sn’." The skew-normal and skew-t distributions (2015): 1-3.
#'
#' @return Numeric value for the expected-shortfall given the distribution and the alpha risk.
#' @export
#'
#' @examples
#' f_ES(0.95, "gaussian", params=c(0,1))
#' f_ES(0.95, "gaussian", params=c(0,1), accuracy=1e-05)
#' f_ES(0.9999, "gaussian", params=c(0,1), accuracy=1e-04)
#'
f_ES <- function(alpha, type_function, params, accuracy=5e-03){
  ES <- 0

  if (alpha <= 0 || alpha > 1-1e-04) {
    stop("expected shortfall is computable for any 'alpha' less than 0.9999. So, 'alpha' must be a value between 0 and 0.9999.")
  }else{
  x <- seq(alpha, 1-accuracy, by=accuracy)
  }
  if(type_function=="gaussian"){
    ES <- sapply(x, f_VaR, type_function, params=params,simplify=TRUE)
  }else if(type_function=="skew-t"){
    ES <- sapply(x, f_VaR, type_function, params=params,simplify=TRUE)
  }else{
    stop("'type_function' has to be 'gaussian' or 'skew-t'")
  }
  mean(ES)
  return(mean(ES))
}

