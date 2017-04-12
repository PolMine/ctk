setGeneric("checkVrt", function(.Object, ...)  standardGeneric("checkVrt"))

.checkVrtWorker <- function(filename, sourceDir, ...){
  doc <- scan(file.path(sourceDir, filename), what="character", sep="\n", quiet=TRUE)
  Encoding(doc) <- "UTF-8"
  openTextTag <- length(grep("^<text", doc))
  closingTextTag <- length(grep("^</text", doc))
  ifelse(
    openTextTag - closingTextTag != 0,
    FALSE,
    TRUE
  )
}

setMethod("checkVrt", "pipe", function(
  .Object, sourceDir,
  mc=TRUE, progress=FALSE, sample=FALSE, files=NULL, verbose=TRUE, ...){
  dirApply(
    f=.checkVrtWorker,
    x=file.path(.Object@projectDir, sourceDir),
    y=NULL, pattern="vrt",
    mc=mc, progress=progress, sample=sample, files=files, verbose=verbose, ...
    )
})
