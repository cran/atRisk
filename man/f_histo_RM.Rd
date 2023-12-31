% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_histo_RM.R
\name{f_histo_RM}
\alias{f_histo_RM}
\title{Historical parameters and Risk Measures}
\usage{
f_histo_RM(qt_trgt, v_dep, v_expl, type_function, starting_values, alpha)
}
\arguments{
\item{qt_trgt}{Numeric vector, dim k, of k quantiles for different qt-estimations (k>=4)}

\item{v_dep}{Numeric vector of the dependent variable}

\item{v_expl}{Numeric vector of the (k) explanatory variable(s)}

\item{type_function}{String argument : "gaussian" for normal distribution or "skew-t" for t-student distribution}

\item{starting_values}{Numeric vector with initial values for optimization}

\item{alpha}{Numeric argument for Expected-Shortfall, between 0 and 1}
}
\value{
A list with historical estimated coefficients, VaR(alpha) and ES(alpha)
}
\description{
This function allows to calculate historical historical parameters and the VaR and ES for each historical period.
}
\examples{
\donttest{
# Import data
data("data_euro")

# Data process
PIB_euro_forward_4 = data_euro["GDP"][c(5:length(data_euro["GDP"][,1])),]
FCI_euro_lag_4 = data_euro["FCI"][c(1:(length(data_euro["GDP"][,1]) - 4)),]
CISS_euro_lag_4 = data_euro["CISS"][c(1:(length(data_euro["GDP"][,1]) - 4)),]

# for a skew-t
results_s <- f_histo_RM(qt_trgt= as.vector(c(0.10,0.25,0.75,0.90)),
v_dep=PIB_euro_forward_4,
v_expl=cbind(FCI_euro_lag_4, CISS_euro_lag_4),
type_function="skew-t",
starting_values=c(0, 1, -0.5, 1.3),
alpha=0.95)
}

}
