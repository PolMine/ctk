#' @include pipe_class.R
NULL

setGeneric("describeDir", function(object, ...) standardGeneric("describeDir"))

#' @param object a pipe object
#' @param dirDesc a named character vector (names are dirs, values descriptions of the dirs)
#' @rdname pipe
setMethod("describeDir", "pipe", function(object, dirDesc){
  for (dir in names(sourceDir)) object@subdirs[dir] <- sourceDir[[dir]]
  return(object)
})