setGeneric("inspect", function(.Object, ...) standardGeneric("inspect"))

setMethod("inspect", "character", function(.Object, element, attributes, mc=FALSE, verbose=FALSE){
  files <- list.files(.Object, pattern = "xml", full.names = TRUE)
  xpathAttr <- sapply(names(attributes), function(x) paste('@', x, '="', attributes[x], '"', sep="") )
  xpath <- paste('//', element, '[', paste(xpathAttr, collapse=' and ', sep=''), ']', sep='')
  .inspect <- function(file, xpath){
    xml <- xmlTreeParse(file, useInternal=TRUE)
    xmlSApply(getNodeSet(xml, path=xpath), xmlValue)
  }
  if (mc == FALSE) {
    results <- lapply(
      setNames(c(1:length(files)), files),
      function(x){
        if (verbose == FALSE) .progressBar(x, length(files))
        .inspect(files[x], xpath)
      })
  } else {
    results <- mclapply(setNames(c(1:length(files)), files), function(file) .inspect(files[x], xpath) )
  }
  for (i in rev(which(lapply(results, length) == 0))) results[[i]] <- NULL
  results
})