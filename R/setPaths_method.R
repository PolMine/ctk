setGeneric("setPaths", function(.Object) standardGeneric("setPaths"))

#' @exportMethod setPaths
setMethod("setPaths", "pipe", function(.Object){
  if ("CORPUS_REGISTRY" %in% names(Sys.getenv())) .Object@registry <- Sys.getenv("CORPUS_REGISTRY")
  if ("PATH_SAXON" %in% names(Sys.getenv())) .Object@saxonPath <- Sys.getenv("PATH_SAXON")
  if ("PATH_TREETAGGER" %in% names(Sys.getenv())) .Object@treetaggerPath <- Sys.getenv("PATH_TREETAGGER")
  return(.Object)
})

