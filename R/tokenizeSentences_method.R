#' Tokenize sentences.
#' 
#' @param .Object bla
#' @param targetDir the target directory
#' 
#' @param ... further parameters that are passed into \code{dirApply}
#' @param targetElement where to finde the text nodes
#' @param para logical, whether to annotate paragraphs
#' @return the output of the procedure
#' @exportMethod tokenizeSentences
#' @author Andreas Blaette
#' @rdname tokenizeSentences
setGeneric("tokenizeSentences", function(.Object, ...) standardGeneric("tokenizeSentences"))

#' @rdname tokenizeSentences
setMethod("tokenizeSentences", "character", function(
  .Object, targetDir, targetElement = "p", para = FALSE, ...
  ){
  .tokenizeSentences <- function(filename, sourceDir, verbose, targetDir, pyFile){
    cmd <- c("python", pyFile, sourceDir, filename, targetDir, targetElement, as.character(para))
    cmd <- paste(cmd, collapse=" ")
    system(cmd,intern=TRUE)
  }
  retval <- .iterateFunctionFiles(
    sourceDir = .Object, f = .tokenizeSentences, pattern="xml",
    targetDir=targetDir,
    pyFile = system.file("sentenceAnno.py", package="ctk"),
    ...
    )
  retval
})
