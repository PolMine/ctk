% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regexContext_function.R
\docType{methods}
\name{regexContext}
\alias{regexContext}
\alias{regexContext,character-method}
\title{get context of a regex}
\usage{
regexContext(.Object, ...)

\S4method{regexContext}{character}(.Object, regex, ncharContext,
  filenames = NULL, ...)
}
\arguments{
\item{.Object}{bla}

\item{...}{futher parameters that are passed into \code{dirApply}}

\item{regex}{a regex}

\item{ncharContext}{characters to the left and to the right}

\item{filenames}{defaults to NULL, then all files will be examined, of provided, only specific files}
}
\description{
Get a data frame with the left and right context of the match of a regex.
}
