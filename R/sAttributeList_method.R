#' @include ctkPipe_class.R
NULL


setGeneric("sAttributeList", function(.Object, ...) standardGeneric("sAttributeList"))

#' get a list with sAttributes from files
#' 
#' @param object a directory
#' @param mc whether to use multicore
#' @rdname sAttributeList-method
#' @exportMethod sAttributeList
setMethod("sAttributeList", "character", function(.Object, ...){
  .getSAttributes <- function(filename, sourceDir, targetDir=NULL, verbose=FALSE, param=list()){
    elementSummary <- xmlElementSummary(file.path(sourceDir, filename))
    sAttr <- lapply(
      setNames(names(elementSummary$nodeCounts), names(elementSummary$nodeCounts)),
      function(elementName){
        if (elementName %in% names(elementSummary$attributes)){
          retval <- names(elementSummary$attributes[[elementName]])
        } else {
          retval <- NA
        }
        retval
      })
  }
  docs <- dirApply(
    f=.getSAttributes, sourceDir=.Object, targetDir=NULL, verbose=FALSE, param=list(),
    ...
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


#' @import XML
#' @exportMethod sAttributeList
#' @rdname ctkPipe
setMethod("sAttributeList", "ctkPipe", function(.Object, sourceDir, mc=TRUE){
  sAttributes <- sAttributeList(
    file.path(object@projectDir, sourceDir), mc=mc
  )
  object@sAttributes <- sAttributes
  return(object)
})


