setGeneric("regex", function(.Object, ...) standardGeneric("regex") )

#' Get Matches for Regular Expression in Files in Directory.
#' 
#' @param .Object a directory
#' @param pattern a regular expression passed into \code{list.files} to filter files
#' @param regex regular expression passed into \code{grep)
#' @importFrom pbapply pblapply
setMethod("regex", "character", function(.Object, pattern = ".*\\.xml", regex = "<.*?>"){
  hits <- pbapply::pblapply(
    list.files(path = .Object, pattern, full.names=TRUE),
    function(file) grep(regex, readLines(con = file), value = TRUE)
    )
  unlist(hits)
})