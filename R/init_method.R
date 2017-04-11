setGeneric("init", function(object, ...) standardGeneric("init"))

#' @exportMethod init
#' @rdname pipe
setMethod("init", "pipe", function(object){
  object <- ctk:::.subdirs(object)
  object <- ctk:::.setPaths(object)
  object
})

