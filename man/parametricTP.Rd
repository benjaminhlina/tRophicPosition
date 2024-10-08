% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parametricTP.R
\name{parametricTP}
\alias{parametricTP}
\title{Parametric trophic position}
\usage{
parametricTP(siData, lambda = 2, print = TRUE)
}
\arguments{
\item{siData}{an isotopeData class object.}

\item{lambda}{numerical value representing trophic level of baseline(s).}

\item{print}{a logical value to indicate whether the output is printed or
not.}
}
\value{
a list with parametric trophic position calculated with a one
baseline model, a two baselines model and its alpha value, and a two
baselines full model and its alpha value.
}
\description{
Calculation of parametric trophic position (with means) partially based on
Post (2002: Using Stable Isotopes to Estimate Trophic Position: Models,
Methods, and Assumptions. Ecology 83, 703).
}
\details{
In case of the one baseline model, trophic position is calculated as \deqn{TP
= lambda + ((dNc - dNb1) / deltaN)} where lambda is trophic level of baseline
1, dNc are d15N values of consumer, dNb1 are d15N values of baseline 1 and
deltaN are trophic discrimination factor values of N.

In case of the two baselines model, trophic position is calculated as
\deqn{TP = lambda + ((dNc - ((dNb1 * alpha) + (dNb2 * (1 - alpha)))) /
deltaN)} and \deqn{alpha = (dCc - dCb2) / (dCb1 - dCb2)} Additional variables
are dCc (d13C values of consumer), dNb2 (d15N values of baseline 2), alpha
(relative contribution of N from baseline 1), and dCb1 and dCb2 (d13C values
of baselines 1 and 2 respectively).

In case of the two baselines full model, trophic position is calculated with
the same equation as the two baselines model, but alpha is calculated as
\deqn{alpha = ((dCc - (deltaC * TP / lambda)) - dCb2) / (dCb1 - dCb2)} and
includes deltaC (trophic discrimination factor for C).

In all cases trophic position is calculated based on means of isotope values
and trophic discrimination factors. For the two baselines full model, an
iteration is needed to get convergence of trophic position, starting with
alpha calculated with the two baselines simple model. If no convergence is
gotten after 50 iterations a message is plotted and both alpha and trophic
position are printed.
}
\examples{
consumer <- generateTPData()
parametricTP(consumer)
}
