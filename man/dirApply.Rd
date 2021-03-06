% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dirApply_function.R
\name{dirApply}
\alias{dirApply}
\title{Apply a function over files in directory.}
\usage{
dirApply(f, sourceDir, targetDir = NULL, pattern = NULL, mc = FALSE,
  progress = TRUE, verbose = FALSE, sample = FALSE, filenames = NULL,
  continue = FALSE, failsafe = FALSE, param = list())
}
\arguments{
\item{f}{function that will be applied to all files in sourceDir}

\item{sourceDir}{source directory with files to be processed}

\item{targetDir}{target directory where the processed files will be saved}

\item{pattern}{pattern (can be a regex) that will be used by \code{list.files} as 'pattern'-argument}

\item{mc}{logical, whether to use multicore/parallel processing, if numeric, the number of cores to use}

\item{progress}{logical, whether to show progress bar}

\item{verbose}{logical, whether to be verbose}

\item{sample}{defults to FALSE, if a numeric, a number of random files to process}

\item{filenames}{character vector with filenames to be used, a subset of files in \code{sourceDir}}

\item{continue}{work only on those files not yet present in target dir}

\item{failsafe}{whether to be robust and catch errors using \code{try}, available only without parallelisation}

\item{param}{a list with parameters to be passed to the function f}
}
\description{
The function (f) will be applied to the files in a directory (in a lapply/sapply/vapply-style).
}
\details{
Function f passes in a function that will be applied to the individual files
in the source directory (sourceDir). The function needs to conform to a
standardized format, parameters required are "filename" (character),
"sourceDir" (character), "targetDir" (character) and "param" (list).

The function f can be applied in a multicore mode. If progress is \code{FALSE}, then
\code{mclapply} will be used. If progress is \code{TRUE}, \code{pblapply} from
the package \code{pbapply} will be used.
}
