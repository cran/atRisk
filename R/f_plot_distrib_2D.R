#' Plot of historical distributions in 2D
#'
#' @param m_param_histo Numeric matrix containing the parameters of the f_param_histo function
#' @param type_function String argument specifying the distribution type ("gaussian" or "skew-t")
#' @param database Dataframe containing the data, with dates in the first column and dependent variable in the second column
#' @param x_lab String optional argument for the x axis title (default value = x)
#' @param y_lab String optionalargument for the y axis title (default value = y)
#' @param x_min Numeric optional argument (default value = -15)
#' @param x_max Numeric optional argument (default value = 10)
#'
#'
#' @return A plot of historical distributions with the median, four quantiles (5th, 25th, 75th, 95th) and the realized dependent variable.
#' @import ggplot2
#' @export
#' @description
#' This function allows to create a plot in 2D of historical distributions.
#'
#' @examples
#' # Import data
#' data(data_US)
#'
#' data(data_param_histo)
#'
#' results_plot_2D <- f_plot_distrib_2D(m_param_histo=data_param_histo,
#' type_function="skew-t",
#' database=data_US,
#' x_lab="US GDP variation",
#' y_lab="Year")
#'

f_plot_distrib_2D <- function(m_param_histo, type_function, database, x_lab, y_lab, x_min, x_max){

  # quantiles for the plot 2D
  v_quantile <- c(0.05,0.25,0.50,0.75,0.95)

  # Calculation of VaR for quantiles
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
    stop("the 'type_function' has to be 'gaussian' or 'skew-t'")
  }

  dates <- as.Date(database[,1],format="%d/%m/%Y")

  results <- data.frame(VaR_results, dates[-c(1)], database[-c(1),2])
  names(results)<-c("Q05","Q25","Q50","Q75","Q95","t","real")


  # Calculation of x_min and x_max if missing
  if (missing(x_min) && missing (x_max)){
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

  p_0 <- ggplot()+
    geom_ribbon(data=results, aes(xmin=results[,1], xmax=results[,5], y=t),fill="gray30", alpha=0.2) +
    geom_ribbon(data=results, aes(xmin=results[,2], xmax=results[,4], y=t),fill="gray30", alpha=0.2) +
    geom_path(data=results,aes(x=results[,7],y=t, color="realized"),group=2, linewidth = 0.5) +
    geom_path(data=results,aes(x=results[,3],y=t, color="median"), linewidth = 0.4) +
    geom_path(data=results,aes(x=results[,5],y=t, color="Q5th & Q95th"), linewidth = 0.05) +
    geom_path(data=results,aes(x=results[,1],y=t, color="Q5th & Q95th"), linewidth = 0.05) +
    geom_path(data=results,aes(x=results[,2],y=t, color="Q25th & Q75th"), linewidth = 0.4) +
    geom_path(data=results,aes(x=results[,4],y=t, color="Q25th & Q75th"), linewidth = 0.4) +
    scale_color_manual(values=c("khaki1", "#bd8e42", "black", "#876b3a"))+
    labs(x = x_lab,
         y = y_lab,
         color = "") +
    theme(legend.title = element_text(color = "black", size = 8),
          legend.text = element_text(color = "black", size = 6),
          legend.key.size=unit(0.25, 'cm'))+
    xlim(x_min, x_max)

  return(p_0)
}
