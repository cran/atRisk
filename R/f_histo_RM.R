#' Historical parameters and Risk Measures
#'
#' @param qt_trgt Numeric vector, dim k, of k quantiles for different qt-estimations (k>=4)
#' @param v_dep Numeric vector of the dependent variable
#' @param v_expl Numeric vector of the (k) explanatory variable(s)
#' @param type_function String argument : "gaussian" for normal distribution or "skew-t" for t-student distribution
#' @param starting_values Numeric vector with initial values for optimization
#' @param alpha Numeric argument for Expected-Shortfall, between 0 and 1
#'
#' @description
#' This function allows to calculate historical historical parameters and the VaR and ES for each historical period.
#'
#' @return A list with historical estimated coefficients, VaR(alpha) and ES(alpha)
#' @export
#'
#' @examples
#' \donttest{
#' # Import data
#' data("data_euro")
#'
#' # Data process
#' PIB_euro_forward_4 = data_euro["GDP"][c(5:length(data_euro["GDP"][,1])),]
#' FCI_euro_lag_4 = data_euro["FCI"][c(1:(length(data_euro["GDP"][,1]) - 4)),]
#' CISS_euro_lag_4 = data_euro["CISS"][c(1:(length(data_euro["GDP"][,1]) - 4)),]
#'
#' # for a skew-t
#' results_s <- f_histo_RM(qt_trgt= as.vector(c(0.10,0.25,0.75,0.90)),
#' v_dep=PIB_euro_forward_4,
#' v_expl=cbind(FCI_euro_lag_4, CISS_euro_lag_4),
#' type_function="skew-t",
#' starting_values=c(0, 1, -0.5, 1.3),
#' alpha=0.95)
#' }
#'
f_histo_RM <- function(qt_trgt, v_dep, v_expl, type_function, starting_values, alpha){


  if (is.vector(v_dep)==FALSE){
    stop("'v_dep' has to be a vector")
  }else{
    nb_T <- length(v_dep)
  }

  param_histo <- f_param_histo(qt_trgt, v_dep, v_expl, type_function, starting_values)

  VaR_res <- as.vector(rep(0,nb_T))
  ES_res <- as.vector(rep(0,nb_T))
  if(type_function=="gaussian"){
    for (count in 1:nb_T){
      VaR_res[count] <- f_VaR(alpha, "gaussian", params=param_histo[count,2:3])
      ES_res[count] <- f_ES(alpha, "gaussian", params=param_histo[count,2:3])
    }
  }else if(type_function=="skew-t"){
    for (count in 1:nb_T){
      VaR_res[count] <- f_VaR(alpha, "skew-t", params=param_histo[count,2:5])
      ES_res[count] <- f_ES(alpha, "skew-t", params=param_histo[count,2:5])
    }
  }else{
    stop("the 'type_function' has to be 'gaussian' or 'skew-t'")
  }
  results <- list("param_histo"=param_histo, "VaR"=VaR_res, "ES"=ES_res)
  return(results)
}
