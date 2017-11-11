#' R-Package 'ctk' (Corpus Toolkit).
#' 
#' Tools for corpus preparation.
#' 
#' The ctk-package relies on some external tools, such as the TreeTagger. The package gets
#' the information on the location of these tools from environment variables that
#' can be set in the .Renviron file in the home directory of a user.
#' The .Renviron file might might contain the following lines:
#' PATH_SAXON='/opt/saxon/saxon9he.jar'
#' PATH_TREETAGGER='/opt/treetagger'
#' 
#' The saxon XSLT parser ist available here: http://sourceforge.net/projects/saxon/files/
#' For the TreeTagger, see: http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/
#' 
#' @aliases ctk-package ctk
#' @docType package
#' @name ctk
#' @aliases ctk-package
#' @rdname ctk-package
#' @author Andreas Blaette
#' @import methods
#' @examples
#' \dontrun{
#' taz <- new("pipe", projectDir = "/home/blaette/Data/pipeDirs/taz")
#' taz <- setPaths(taz)
#' filesCopied <- getFiles(
#'   taz, sourceDir = "/home/blaette/Lab/rsync/taz/html_out", targetDir = "xml",
#'   pattern = "xml", method = "list.files", recursive = TRUE, rectify = FALSE,
#'  verbose = FALSE, progress = TRUE
#'  )
#' tokenize(taz, sourceDir = "xml", targetDir = "tok", progress = TRUE, mc = 3)
#' treetagger(taz, sourceDir = "tok", "vrt", progress = TRUE, mc = 3)
#' fix(taz, sourceDir = "vrt", targetDir = "vrt2", mc = 8)
#' encode(taz, corpus = "taz2", sourceDir = "vrt5", sample = 500, embedding = "10", encoding = "utf8")
#' }
NULL


#' regexPostprocessing
#' 
#' @name regexPostprocessing
#' @docType data
#' @format a list
#' @keywords datasets
NULL

#' @name timePerFile-class
#' @title Time per file.
#' @description Class and methods to report time consumed to process
#' files.
#' @exportClass timePerFile
#' @param x object to show/pring
#' @param ... further parameters (not used)
#' @rdname timePerFile
#' @aliases timePerFile_class
setOldClass("timePerFile")


#' @export print.timePerFile
#' @S3method print timePerFile
#' @rdname timePerFile
print.timePerFile <- function(x, ...){
  meanTime <- round(mean(unlist(lapply(x, as.numeric))), 2)
  cat("Number of files processed: ", length(x), "\n")
  cat("Average time per file:     ", meanTime)
}

