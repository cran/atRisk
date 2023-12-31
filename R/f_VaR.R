#' Value-at-Risk
#'
#' @param alpha Numeric argument for Expected-Shortfall, between 0 and 1
#' @param type_function String argument : "gaussian" for normal distribution or "skew-t" for t-student distribution
#' @param params Numeric vector containing parameters of the distribution
#'
#' @description
#' The function allows to calculate Value-at-Risk for a given distribution. It takes as parameters alpha (risk level), a distribution and the parameters associated with this distribution. For example, for a normal distribution, the user must enter the mean and the standard deviation. Currently, the function can calculate the Value-at-Risk for the normal distribution and for the skew-t distribution (Azzalini and Capitianio, 2003)
#'
#' @references Azzalini, Adelchi, and Antonella Capitanio. "Distributions generated by perturbation of symmetry with emphasis on a multivariate skew t‐distribution." Journal of the Royal Statistical Society: Series B (Statistical Methodology) 65.2 (2003): 367-389.
#' @references Azzalini, Adelchi, and Maintainer Adelchi Azzalini. "Package ‘sn’." The skew-normal and skew-t distributions (2015): 1-3.
#'
#' @return Numeric value for the Value-at-Risk given the distribution and the alpha risk
#' @export
#'
#' @examples
#' f_VaR(0.95, "gaussian", params=c(0,1))
#'
f_VaR <- function(alpha, type_function, params){
  # error management and match function
  if (alpha <= 0 || alpha >= 1) {
    stop("'alpha' must be a value between 0 and 1.")
  }else{
  }
  
  if(type_function=="gaussian"){
    if (length(params)!=2){
      stop("for a Gaussian distribution, 'params' must contain 2 values")
    }else{
    }
    results<-qnorm(1 - alpha,params[1],params[2])
  }else if(type_function=="skew-t"){
    if (length(params)!=4){
      stop("for a skew-t distribution, 'params' must contain 4 values")
    }else{
    }
    results<- qst(1 - alpha,xi=params[1],omega=params[2],alpha=params[3], nu=params[4],tol = 1e-04)
  } else {
    stop("the distribution has to be 'gaussian' or 'skew-t'")
  }
  return(results)
}
