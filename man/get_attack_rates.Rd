% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_attack_rates.R
\name{get_attack_rates}
\alias{get_attack_rates}
\title{Multi-annual model of infection and vaccination (version 2)}
\usage{
get_attack_rates(inf_history, ages_mat = NULL, vac_history = NULL, years)
}
\arguments{
\item{inf_history}{matrix of infection histories for each person}

\item{ages_mat}{matrix of ages of each person for every year}

\item{vac_history}{matrix of vaccination history for each person}

\item{years}{vector of years to run simulation over (YYYY format)}
}
\value{
data frame or list of data frames with attack rates
}
\description{
This function initializes the population before running the model.
}
\keyword{morevac}
