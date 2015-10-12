setGeneric("characterCount", function(.Object, ...) standardGeneric("characterCount"))

#' count characters in files
#' 
#' @param .Object a directory with XML files
#' @param regexCharsToKeep a regex defining the characters to keep
#' @param toLower logical, whether to to apply tolower to string read in
#' @param progress defaults to TRUE
#' @param mc defaults to FALSE
#' @param verbose whether to be verbose
#' @import XML
#' @export characterCount
setMethod("characterCount", "character", function(.Object, regexCharsToKeep="[a-zA-Z]", xml=TRUE, toLower=FALSE, progress=TRUE, mc=FALSE, verbose=TRUE){
  if (verbose == TRUE) message("... counting characters")
  .characterCount <- function(filename, sourceDir, targetDir, verbose, param=list(xml=TRUE, toLower=TRUE)){
    if (param$xml == TRUE){
      doc <- getChildrenStrings(xmlParse(file.path(sourceDir, filename)))
    } else if (param$xml == FALSE){
      doc <- paste(scan(file.path(sourceDir, filename), quiet=TRUE, what="character", sep="\n"), collapse="\n")
    }
    if ( param$toLower == TRUE ) doc <- tolower(doc)
    table(unlist(strsplit(doc, "")))
  }
  charCountList <- dirApply(
    f=.characterCount, sourceDir=.Object, targetDir=NULL,
    progress=progress, verbose=verbose, mc=mc, param=list(xml=xml, toLower=toLower)
  )
  charCount <- tapply(
    unname(unlist(charCountList)),
    INDEX=unlist(sapply(charCountList, function(x) names(x))),
    FUN=sum
  )
  if(is.null(regexCharsToKeep)){
    charCountFiltered <- charCount
  } else {
    charCountFiltered <- charCount[grep(regexCharsToKeep, names(charCount))]
  }
  return(charCountFiltered)
})
