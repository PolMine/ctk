setGeneric("getNgrams", function(.Object, ...) standardGeneric("getNgrams"))

#' get ngrams 
#' 
#' @param .Object the source directory
#' @param chars chars to keep, if NULL (default, all chars are kept)
#' @param returnSparseMatrix logical, whether to return a TermDocumentMatrix, defaults to TRUE. If FALSE, a list will be returned
#' @param progress logical
#' @param verbose logical
#' @param mc logical, or the number of cores
#' @import XML
#' @importFrom slam simple_triplet_matrix
#' @examples 
#' \dontrun{
#' xmlDir <- "/Users/blaette/Lab/repos/keywords/data/figaro/xml"
#' noChars <- characterCount(xmlDir, toLower=TRUE, progress=TRUE, verbose=TRUE, mc=3)
#' ngramMatrix <- getNgrams(xmlDir, charCount=noChars, nChar=10, progress=TRUE, mc=3, verbose=TRUE)
#' }
setMethod("getNgrams", "character", function(.Object, chars=NULL, returnSparseMatrix=TRUE, progress=TRUE, verbose=FALSE, mc=FALSE){
  charsToKeep <- chars
  .ngramCount <- function(filename, sourceDir, targetDir, verbose, param){
    charsToKeep <- param$charsToKeep
    n <- 5
    xmlAsString <- getChildrenStrings(xmlParse(file.path(sourceDir, filename)))
    if (!is.null(charsToKeep)){
      charSauceRaw <- sapply(unlist(strsplit(xmlAsString, "")), function(x) ifelse(x %in% charsToKeep, x, NA))
      charSauce <- paste(charSauceRaw[which(!is.na(charSauceRaw))], sep="", collapse="")
    } else {
      charSauce <- paste(unlist(strsplit(xmlAsString, "")), sep="", collapse="")
    }
    ngrams <- sapply(c(1:(nchar(charSauce)-n+1)), function(x) substr(charSauce, x, x+n-1) )
    table(ngrams)
  }
  ngramList <- dirApply(
    f=.ngramCount, sourceDir=.Object, targetDir=NULL,
    progress=progress, verbose=verbose, mc=mc, param=list(charsToKeep=charsToKeep)
  )
  if (returnSparseMatrix == TRUE){
    ngramsFactor <- as.factor(unlist(lapply(ngramList, function(x) names(x))))
    ngramMatrix <- simple_triplet_matrix(
      i=as.numeric(ngramsFactor),
      j=unlist(lapply(c(1:length(ngramList)), function(x) rep(x, times=length(ngramList[[x]])))),
      v=unname(unlist(ngramList)),
      ncol=length(ngramList),
      nrow=length(levels(ngramsFactor)),
      dimnames=list(Terms=levels(ngramsFactor), Docs=names(ngramList))
    )
    class(ngramMatrix) <- c("TermDocumentMatrix", "simple_triplet_matrix")
    retval <- ngramMatrix
  } else {
    retval <- ngramList
  }
  return(retval)
})

