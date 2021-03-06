% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_or_install.r
\name{load_or_install}
\alias{load_or_install}
\title{Loads a list of libraries, installs when necessary}
\usage{
load_or_install(wants)
}
\arguments{
\item{wants}{A list of R modules available on CRAN}
}
\description{
Loads a list of libraries, installs when necessary
}
\examples{
load_or_install(c('tidyverse','Hmisc','glmnet'))
}
\keyword{install}
\keyword{libraries}
