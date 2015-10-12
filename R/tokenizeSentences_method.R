#' @include ctkPipe_class.R
NULL


setGeneric("tokenizeSentences", function(.Object, ...) standardGeneric("tokenizeSentences"))

setMethod("tokenizeSentences", "character", function(
  .Object, targetDir, targetElement="p", para=FALSE, mc=FALSE, verbose=FALSE, progress=TRUE
  ){
  .tokenizeSentences <- function(filename, sourceDir, verbose, targetDir, pyFile){
    cmd <- c("python", pyFile, sourceDir, filename, targetDir, targetElement, as.character(para))
    cmd <- paste(cmd, collapse=" ")
    system(cmd,intern=TRUE)
  }
  retval <- .iterateFunctionFiles(
    sourceDir=.Object, f=.tokenizeSentences, pattern="xml",
    mc=mc, verbose=verbose, progress=progress, targetDir=targetDir,
    pyFile=system.file("sentenceAnno.py", package="ctk")
    )
  retval
})

#' @param targetElement where to finde the text nodes
#' @param para logical, whether to annotate paragraphs
#' @return the output of the procedure
#' @exportMethod tokenizeSentences
#' @author Andreas Blaette
#' @rdname ctkPipe
setMethod("tokenizeSentences", "ctkPipe", function(
  .Object, sourceDir="xml",targetDir="xmlAnno",
  targetElement="p", para=FALSE,
  mc=FALSE, verbose=FALSE, progress=TRUE
){
  checkDirs(.Object, sourceDir, targetDir)
  tokenizeSentences(
    .Object=file.path(.Object@projectDir, sourceDir), targetDir=file.path(.Object@projectDir, targetDir),
    targetElement=targetElement, para=para, mc=mc, verbose=verbose, progress=progress
    )
  return(.Object)
})


