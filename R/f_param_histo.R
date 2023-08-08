#' Historical parameters
#'
#' @param qt_trgt Numeric vector, dim k, of k quantiles for different qt-estimations (k>=4)
#' @param v_dep Numeric vector of the dependent variable
#' @param v_expl Numeric vector of the (k) explanatory variable(s)
#' @param type_function String argument : "gaussian" for normal distribution or "skew-t" for t-student distribution
#' @param starting_values Numeric vector with initial values for optimization
#'
#' @description
#' This function allows to calculate historical parameters of distributions.
#'
#' @return A matrix with the historical parameters of the distribution
#' @export
#'
#' @examples
#' \donttest{
#' # Import data
#' data("data_US")
#'
#' # Data process data_US
#' PIB_us_forward_1 = data_US["GDP"][c(2:length(data_US["GDP"][,1])),]
#' NFCI_us_lag_1 = data_US["NFCI"][c(1:(length(data_US["GDP"][,1]) - 1)),]
#'
#' # Historical parameters for a skew-t distribution
#' results_s <- f_param_histo(qt_trgt= as.vector(c(0.10,0.25,0.75,0.90)),
#' v_dep=PIB_us_forward_1,
#' v_expl=NFCI_us_lag_1,
#' type_function="skew-t",
#' starting_values=c(0, 1, -0.5, 1.3))
#' }
#'
f_param_histo <- function(qt_trgt, v_dep, v_expl, type_function, starting_values){

  if (is.vector(v_dep)==FALSE){
    stop("'v_dep' has to be a vector")
  }else{
    nb_T <- length(v_dep)
  }

  if(type_function=="gaussian"){
    param_histo <- matrix(data=0,ncol=3, nrow=nb_T)
  }else if(type_function=="skew-t"){
    param_histo <- matrix(data=0,ncol=5, nrow=nb_T)
  }else{
    stop("the 'type_function' has to be 'gaussian' or 'skew-t'")
  }

  for (ct_period in 1:nb_T){

    res_qt_reg <- f_compile_quantile(qt_trgt, v_dep, v_expl, t_trgt = ct_period)
    results_distrib <- f_distrib(type_function, compile_qt=res_qt_reg, starting_values)
    if(type_function=="gaussian"){
      param_histo[ct_period,1] <- ct_period
      param_histo[ct_period,2] <- results_distrib$mean
      param_histo[ct_period,3] <- results_distrib$sd
    }else if(type_function=="skew-t"){
      param_histo[ct_period,1] <- ct_period
      param_histo[ct_period,2] <- results_distrib$xi
      param_histo[ct_period,3] <- results_distrib$omega
      param_histo[ct_period,4] <- results_distrib$alpha
      param_histo[ct_period,5] <- results_distrib$nu
    }else{
    }
  }

  return(param_histo)
}
