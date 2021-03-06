% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/characterCount_method.R
\name{characterCount}
\alias{characterCount}
\title{Count characters in files.}
\usage{
characterCount(.Object, regexCharsToKeep = "[a-zA-Z]", xml = TRUE,
  toLower = FALSE, decreasing = TRUE, verbose = TRUE, ...)
}
\arguments{
\item{.Object}{a directory with XML files}

\item{regexCharsToKeep}{a regex defining the characters to keep}

\item{xml}{logical, whether filename is a XML file, or not}

\item{toLower}{logical, whether to to apply tolower to string read in}

\item{decreasing}{logical, whether sort order is decreasing, or increasing, see documentation
for \code{order}}

\item{verbose}{logical, whether to be talkative}

\item{...}{further parameters that will be passed into \code{dirApply}}
}
\description{
Count characters in files.
}
