setGeneric("init", function(object, ...) standardGeneric("init"))

#' @exportMethod init
#' @rdname ctkPipe
setMethod("init", "ctkPipe", function(object){
  object <- ctk:::.subdirs(object)
  object <- ctk:::.setPaths(object)
  object
})

