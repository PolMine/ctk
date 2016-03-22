# @include ctkPipe_class.R
NULL

#' @exportMethod consolidate
setGeneric("consolidate", function(.Object, ...) standardGeneric("consolidate"))

.consolidate <- function(filename, sourceDir, targetDir, verbose, param){
  startTime <- Sys.time()
  consolidation <- param[["consolidation"]]
  doc <- xmlTreeParse(file.path(sourceDir, filename), useInternalNodes=TRUE)
  nodes <- getNodeSet(doc, paste("//", param[["element"]]))
  for (i in c(1:length(nodes))){
    attrValuesOld <- xmlAttrs(nodes[[i]])
    if (attrValuesOld[param[["attribute"]]] %in% names(consolidation)){
      attrValuesOld[param[["attribute"]]] <- consolidation[[ attrValuesOld[param[["attribute"]]] ]]
      xmlAttrs(nodes[[i]]) <- attrValuesOld
    }
  }
  xmlOut <- saveXML(doc, prefix='<?xml version="1.0" encoding="UTF-8"?>', indent=TRUE, encoding="UTF-8")
  if (is.null(targetDir)){
    return(xmlOut)
  } else {
    cat(xmlOut, file=file.path(targetDir, filename))
    return(as.character(Sys.time() - startTime))
  }
}


setMethod("consolidate", "ctkPipe", function(
  .Object, sourceDir, targetDir,
  element, attribute, consolidation,
  ...
  ){
  checkDirs(.Object, sourceDir, targetDir)
  dirApply(
    f=.consolidate,
    sourceDir=file.path(.Object@projectDir, sourceDir),
    targetDir=file.path(.Object@projectDir, targetDir),
    param=list(consolidation=consolidation, element=element, attribute=attribute),
    ...
    )
})
