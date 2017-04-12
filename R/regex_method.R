#' @include pipe_class.R
NULL


setGeneric("regex", function(object, ...){standardGeneric("regex")})

#' apply regex to a set of files
#' 
#' @param object a path
#' @param subdir provide a subdirectory
#' @param pattern what to find
#' @param regex regex to be applied  
setMethod("regex", "character", function(object, subdir=NULL, pattern=".*\\.xml", regex="<.*?>"){
  if (!is.null(subdir)){
    dir <- file.path(object, subdir)
  } else {
    dir <- object
  }
  hits <- lapply(
    list.files(path=dir, pattern, full.names=TRUE),
    function(file){
      print(file)
      text <- scan(file=file, what="character")
      matches <- grep(regex, text, value=TRUE)
      gsub(paste(".*(", regex, ").*", sep=""), "\\1", matches)
    })
  unique(unlist(hits))
})