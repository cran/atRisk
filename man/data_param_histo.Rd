\name{data_param_histo}
\alias{data_param_histo}
\docType{data}
\title{
Historical parameters (skew-t) for the US from 1973:Q1 to 2022:Q3
}
\description{
Data corresponding to historical parameters estimated over the period 1973:Q1 to 2022:Q3, based on the data_US file in the matrisk package, with the skew-t distribution, and calculated with the f_param_histo function.
data_param_histo_US has been calculated using c(0.05,0.25,0.75,0.95) for the qt_trgt parameter, PIB_us_forward_1 as the dependent variable, NFCI_us_lag_1 as the explanatory variable, "skew-t" for the type_function parameter and c(0, 1, -0.5, 1.3) for the starting_values.
}
\usage{data("data_param_histo")}
\format{
  A matrix with 187 rows and 5 columns (first column for the periods and the 4 following columns for the for parameters of the skew-t distribution).
}
