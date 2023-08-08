#' Plot of historical distributions in 3D
#'
#' @param m_param_histo Numeric matrix containing the parameters of the f_param_histo function
#' @param type_function String argument specifying the distribution type ("gaussian" or "skew-t")
#' @param database Dataframe containing the data, with dates in the first column and dependent variable in the second column
#' @param n_samples Number optional of samples for the plot (default value = 1000)
#' @param x_lab String optional argument for the x axis title (default value = x)
#' @param y_lab String optional argument for the y axis title (default value = y)
#' @param x_min Numeric optional argument (default value = -15)
#' @param x_max Numeric optional argument (default value = 10)
#'
#'
#' @return A plot in 3D of historical distributions
#' @importFrom sn rst
#' @importFrom stats rnorm
#' @import ggplot2
#' @import ggridges
#' @export
#' @description
#' This function allows to create a plot in 3D of historical distributions.
#'
#' @examples
#' # Import data
#' data(data_US)
#'
#' data(data_param_histo)
#'
#' results_plot_3D <- f_plot_distrib_3D(m_param_histo=data_param_histo,
#' type_function="skew-t",
#' database=data_US,
#' x_lab="US GDP variation",
#' y_lab="Year")
#'

f_plot_distrib_3D <- function(m_param_histo, type_function, database, n_samples, x_min, x_max, x_lab, y_lab){

  # initialization of parameters
  dates <- as.Date(database[,1],format="%d/%m/%Y")
  dates <- dates[-length(dates)]
  if (missing(n_samples)){
    n_samples <- 10000
  }
  n_dates <- length(dates)
  samples_3D <- data.frame(matrix(data=0, nrow=n_dates*n_samples, ncol = 4))
  colnames(samples_3D) <- c("date", "month", "X", "mean_value")
  samples_3D[,1] <- rep(dates[c(1 : n_dates)], n_samples)
  samples_3D[,2] <- as.Date(as.Date(samples_3D[,1], format="%Y-%m-%d"))

  if (type_function=="skew-t"){
    for (count_date in c(1:n_dates)){
      samples_3D[seq(count_date, n_samples*n_dates, n_dates), 3] <- rst(n_samples, dp = c(m_param_histo[count_date, 2], m_param_histo[count_date, 3], m_param_histo[count_date, 4], m_param_histo[count_date, 5]))
    }
  }else if (type_function=="gaussian"){
    for (count_date in c(1:n_dates)){
      samples_3D[seq(count_date, n_samples*n_dates, n_dates), 3] <- rnorm(n_samples, c(m_param_histo[count_date, 2], m_param_histo[count_date, 3]))
    }
  }else{
    stop("the 'type_function' has to be 'gaussian' or 'skew-t'")
  }

  mean_values = matrix(data = NA, nrow=n_dates)
  for (count_date in c(1:n_dates)){
    mean_values[count_date] <- mean(samples_3D[which(samples_3D$date == dates[count_date]),3], na.rm = TRUE)
  }

  for (count_date in c(1:n_dates)){
    samples_3D[seq(count_date, n_samples*n_dates, n_dates), 4] <- rep(mean_values[count_date], n_samples)
  }

  # Calculation of x_min and x_max if missing
  if (missing(x_min) && missing(x_max)){
    v_quantile <- c(0.025,0.975)
    VaR_results <- matrix(data=0,nrow=nrow(m_param_histo), ncol=length(v_quantile))
    if(type_function=="gaussian"){
      for(count in 1:nrow(m_param_histo)){
        VaR_results[count,] <- sapply(v_quantile, f_VaR, type_function="gaussian", params=m_param_histo[count,2:3],simplify=TRUE)
      }
    }else if(type_function=="skew-t"){
      for(count in 1:nrow(m_param_histo)){
        VaR_results[count,] <- sapply(v_quantile, f_VaR, type_function="skew-t", params=m_param_histo[count,2:5],simplify=TRUE)
      }
    }else{
    }
    x_min <- floor(min(VaR_results[,2]))
    x_max <- ceiling(max(VaR_results[,1]))
  }

  if (missing(x_lab)){
    x_lab <- "x"
  }
  if (missing(y_lab)){
    y_lab <- "y"
  }

  X <- x <- NULL
  p_1 <- ggplot(samples_3D, aes(x = `X`, y=`date`, group=`date`, fill=after_stat(x))) +
    geom_density_ridges_gradient(scale = 20, size = 0.1, rel_min_height = 0.05) +
    scale_fill_gradient(low = "#bd8e42", high = "#bebfbf") +
    xlim(x_min, x_max) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.key.size = unit(0.25, 'cm'),
          legend.title = element_text(size=8)) +
    labs(x=x_lab, y = y_lab) +
    guides(fill=guide_legend(""))

  return(p_1)

}
