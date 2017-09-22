#' Find and replace.
#' 
#' @param replacements a list
#' @param checkValidity whether to validate XML in targetDir
#' @param progress whether to use progress bar
#' @exportMethod findReplace
setGeneric("findReplace", function(object, ...) standardGeneric("findReplace"))

.findReplace <- function(filename, sourceDir, targetDir, verbose, param){
  startTime <- Sys.time()
  replacements <- param[["replacements"]]
  doc <- scan(
    file=file.path(sourceDir, filename),
    what="character", sep="\n",
    blank.lines.skip=TRUE,
    quiet=TRUE, fileEncoding=param[["encoding"]]
  )
  for (replacement in replacements) doc <- gsub(replacement[1], replacement[2], doc)
  cat(
    doc, sep="\n",
    file=file.path(targetDir, filename)
  )
  return(Sys.time() - startTime)
}
