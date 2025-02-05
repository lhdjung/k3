% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boolean.R
\name{boolean}
\alias{boolean}
\alias{is_boolean_like}
\alias{is_boolean_like_scalar}
\title{Does an object behave as if Boolean?}
\usage{
is_boolean_like(x)

is_boolean_like_scalar(x)
}
\arguments{
\item{x}{Any object.}
}
\value{
Logical (length 1). \code{TRUE} or \code{FALSE}, never \code{NA}.
}
\description{
\itemize{
\item \code{is_boolean_like()} checks if an object is a logical vector that only has
\code{TRUE} and / or \code{FALSE} values, not \code{NA}.
\item \code{is_boolean_like_scalar()} additionally checks if the object has length 1:
is it a single \code{TRUE} or \code{FALSE} value?
}
}
\details{
Although R uses three-valued logic instead of Booleans, it is often
useful to emulate Boolean logic by excluding \code{NA}.

\code{is_boolean_like_scalar(x)} is equivalent to \code{isTRUE(x) || isFALSE(x)} but
tends to be faster if the result is likely \code{FALSE}.
}
