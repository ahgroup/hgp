% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-de-chunker.R
\name{os_call}
\alias{os_call}
\title{Run the appropriate bash-command invoker function by OS}
\usage{
os_call(os_detected, ...)
}
\arguments{
\item{os_detected}{a string, usually the result of `.Platform$OS.type`.}

\item{...}{the arguments to pass to either `shell()` or `system()`.}
}
\value{
Whatever is returned by `shell()` or `system()`.
}
\description{
This function constructs a shell call that is appropriate for the system
OS. The system OS is passed as the first argument, so it should be detected
before calling this function or passed manually.
The remaining arguments are the arguments for the shell call, which are the
same regardless of OS for the purposes of the model (de)chunker.
We also implement error handling via purrr::possibly(). The "otherwise" value
in purrr possibly is set to -1 because this is the error code normally
returned by a shell() or system() invocation if a system-level error occurs.
}
