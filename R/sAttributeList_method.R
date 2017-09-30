.getSAttributes <- function(filename, sourceDir, targetDir=NULL, verbose=FALSE, param=list()){
  elementSummary <- xmlElementSummary(file.path(sourceDir, filename))
  toReturn <- lapply(
    setNames(names(elementSummary$nodeCounts), names(elementSummary$nodeCounts)),
    function(elementName){
      if (elementName %in% names(elementSummary$attributes)){
        retval <- names(elementSummary$attributes[[elementName]])
      } else {
        retval <- NA
      }
      retval
    })
  toReturn
}


setGeneric("sAttributeList", function(.Object, ...) standardGeneric("sAttributeList"))

#' get a list with sAttributes from files
#' 
#' @param .Object a directory
#' @param sample numeric, a subsample of files to derive the s-attributes from
#' @param ... further parameters that are passed into \code{dirApply} 
#' @rdname sAttributeList-method
#' @export sAttributeList
#' @import XML
#' @importFrom stats setNames
sAttributeList <- function(.Object, sample = 100, ...){
  docs <- dirApply(
    f = .getSAttributes, sourceDir=.Object, targetDir = NULL, param=list(), sample = sample, ...
    )
  uniqueElements <- unique(unlist(lapply(docs, names)))
  # sAttrAllDocs <- lapply(setNames(uniqueElements, uniqueElements), function(x) NA)
  sAttributeList <- lapply(
    setNames(uniqueElements, uniqueElements),
    function(elementName) {
      unique(unlist(lapply(docs, function(doc) doc[[elementName]])))
    }
  )
  return(sAttributeList)
}




