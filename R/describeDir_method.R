#' @include ctkPipe_class.R
NULL

setGeneric("describeDir", function(object, ...) standardGeneric("describeDir"))

#' @param object a ctkPipe object
#' @param dirDesc a named character vector (names are dirs, values descriptions of the dirs)
#' @rdname ctkPipe
setMethod("describeDir", "ctkPipe", function(object, dirDesc){
  for (dir in names(sourceDir)) object@subdirs[dir] <- sourceDir[[dir]]
  return(object)
})