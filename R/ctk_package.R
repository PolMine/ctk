#' ctk-package
#' 
#' Tools for importing XML into the CWB
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
#' @aliases ctk-package ctk
#' @docType package
#' @name ctk
#' @aliases ctk-package
#' @rdname ctk-package
#' @author Andreas Blaette
NULL


#' regexPostprocessing
#' 
#' @name regexPostprocessing
#' @docType data
#' @format a list
#' @keywords datasets
NULL

#' @exportClass timePerFile
setOldClass("timePerFile")

#' @exportMethod show
setMethod("show", "timePerFile", function(object){
  meanTime <- round(mean(unlist(lapply(object, as.numeric))), 2)
  cat("Number of files processed: ", length(object), "\n")
  cat("Average time per file:     ", meanTime)
})

#' @exportMethod print
setMethod("print", "timePerFile", function(x){
  meanTime <- round(mean(unlist(lapply(x, as.numeric))), 2)
  cat("Number of files processed: ", length(x), "\n")
  cat("Average time per file:     ", meanTime)
})
