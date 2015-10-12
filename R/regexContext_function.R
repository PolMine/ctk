#' get context of a regex
#' 
#' Get a data frame with the left and right context of the match of a regex.
#' 
#' @param .Object bla
#' @param filenames defaults to NULL, then all files will be examined, of provided, only specific files
#' @param regex a regex
#' @param ncharContext characters to the left and to the right
#' @param verbose logical
#' @param progress logical
#' @param mc logical or numeric
#' @importFrom stringi stri_locate_all_regex
#' @exportMethod regexContext
setGeneric("regexContext", function(.Object, ...) standardGeneric("regexContext"))

.regexContext <- function(filename, sourceDir=NULL, targetDir=NULL, verbose=FALSE, param=list(asText=FALSE)){
  regex <- param[["regex"]]
  ncharContext <- param[["ncharContext"]]
  if (param[["asText"]] == FALSE){
    doc <- paste(scan(file=file.path(sourceDir, filename), what="character", sep="\n", quiet=TRUE), collapse="\n")  
  } else {
    doc <- filename
  }
  regexMatchMatrices <- lapply(
    regex,
    function(regex){
      regexResults <- stri_locate_all_regex(doc, regex)
      regexMatchMatrices <- lapply(c(1:length(regexResults)), function(i){
        regexResult <- regexResults[[i]]
        if (all(is.na(unlist(regexResult)))){
          retval <- NULL
        } else {
          retval <- t(apply(regexResult, 1, function(row, ncharContext=25){
            c(
              pos=i,
              regex=regex,
              left_context=substr(doc[i], row[1] - ncharContext, row[1] - 1),
              regex_match=substr(doc[i], row[1], row[2]),
              right_context=substr(doc[i], row[2] + 1, row[2] + ncharContext)
            )
          }))
        }
        retval
      })
      regexMatchMatrix <- do.call(rbind, regexMatchMatrices)
      regexMatchMatrix
    }
  )
  regexMatchMatrix <- do.call(rbind, regexMatchMatrices)
  data.frame(regexMatchMatrix, stringsAsFactors=FALSE)  
}


setMethod("regexContext", "character", function(.Object, regex, ncharContext, ...){
  sourceDir <- .Object
  if (is.null(filenames)) stopifnot(file.info(sourceDir)[1,"isdir"] == TRUE)
  regexResult <- dirApply(
    f=.regexContext, sourceDir=sourceDir, targetDir=NULL, 
    param=list(regex=regex, ncharContext=ncharContext),
    ...
    )
  regexResultWithFilename <- lapply(
    c(1:length(regexResult)),
    function(i) data.frame(filename=names(regexResult)[i], regexResult[[i]])
    )
  data.frame(do.call(rbind, regexResultWithFilename), stringsAsFactors=FALSE)
})
