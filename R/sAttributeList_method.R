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
#' @param object a directory
#' @param mc whether to use multicore
#' @rdname sAttributeList-method
#' @exportMethod sAttributeList
#' @import XML
setMethod("sAttributeList", "character", function(.Object, sample = 100, progress = TRUE, verbose = FALSE, ...){
  docs <- dirApply(
    f = .getSAttributes, sourceDir=.Object, targetDir = NULL, verbose = FALSE, param=list(), sample = sample, progress = progress
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
})




