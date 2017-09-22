#' Tokenize sentences.
#' 
#' @param targetElement where to finde the text nodes
#' @param para logical, whether to annotate paragraphs
#' @return the output of the procedure
#' @exportMethod tokenizeSentences
#' @author Andreas Blaette
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
    sourceDir = .Object, f = .tokenizeSentences, pattern="xml",
    mc=mc, verbose=verbose, progress=progress, targetDir=targetDir,
    pyFile = system.file("sentenceAnno.py", package="ctk")
    )
  retval
})
