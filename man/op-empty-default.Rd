% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vector-funs.R
\name{op-empty-default}
\alias{op-empty-default}
\alias{\%|||\%}
\title{Replace an empty vector with something else. Based on `\%||\%` from rlang.}
\usage{
x \%|||\% y
}
\arguments{
\item{x, y}{if `x` is empty, will return `y`; otherwise returns `x`.}
}
\description{
Replace an empty vector with something else. Based on `\%||\%` from rlang.
}
\examples{
c(1, 2)[1] \%|||\% NA_real_
c(1, 2)[0] \%|||\% NA_real_

}