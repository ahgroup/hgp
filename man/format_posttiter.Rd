% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HAI-helpers.R
\name{format_posttiter}
\alias{format_posttiter}
\title{Do the calculation and formatting for titer outcomes.
Internal helper used for censoring formatting that you shouldn't use
unless you know what you are doing.}
\usage{
format_posttiter(log_post, log_out)
}
\arguments{
\item{log_post}{log-scale vector of post-titers.}

\item{log_out}{Boolean, TRUE if the output should be on the log scale.}
}
\value{
formatted post-titer values.
}
\description{
Do the calculation and formatting for titer outcomes.
Internal helper used for censoring formatting that you shouldn't use
unless you know what you are doing.
}
