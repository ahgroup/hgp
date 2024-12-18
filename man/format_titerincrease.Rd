% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HAI-helpers.R
\name{format_titerincrease}
\alias{format_titerincrease}
\title{Calculation of censoring bounds for TI outcome
Internal helper for censoring data transformation that you should't use
unless you know what you are doing.}
\usage{
format_titerincrease(log_post, log_pre, log_out)
}
\arguments{
\item{log_post}{Vector of log-scale post-vaccination titer values.}

\item{log_pre}{Vector of log-scale pre-vaccination titer values.}

\item{log_out}{Boolean, TRUE if the outcome should be on the log scale.}
}
\value{
A data frame containing the formatted `c`, `y`, and `y2` numeric
vectors representing the titer increase in brms censoring notation.
}
\description{
Calculation of censoring bounds for TI outcome
Internal helper for censoring data transformation that you should't use
unless you know what you are doing.
}
