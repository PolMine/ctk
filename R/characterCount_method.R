.characterCount <- function(filename, sourceDir, targetDir, verbose, param = list(xml = TRUE, toLower = TRUE)){
  if (param[["xml"]] == TRUE){
    doc <- getChildrenStrings(xmlParse(file.path(sourceDir, filename)))
  } else {
    doc <- paste(readLines(file.path(sourceDir, filename)), collapse = "\n")
  }
  if ( param$toLower == TRUE ) doc <- tolower(doc)
  table(unlist(strsplit(doc, "")))
}


#' Count characters in files.
#' 
#' @param .Object a directory with XML files
#' @param regexCharsToKeep a regex defining the characters to keep
#' @param toLower logical, whether to to apply tolower to string read in
#' @param xml logical, whether filename is a XML file, or not
#' @param decreasing logical, whether sort order is decreasing, or increasing, see documentation
#' for \code{order}
#' @param verbose logical, whether to be talkative
#' @param ... further parameters that will be passed into \code{dirApply}
#' @importFrom XML getChildrenStrings xmlParse
#' @export characterCount
characterCount <- function(.Object, regexCharsToKeep = "[a-zA-Z]", xml = TRUE, toLower = FALSE, decreasing = TRUE, verbose = TRUE, ...){
  if (verbose) message("... counting characters")
  charCountList <- dirApply(
    f = .characterCount, sourceDir = .Object, targetDir = NULL,
    param = list(xml = xml, toLower = toLower), ...
  )
  charCount <- tapply(
    unname(unlist(charCountList)),
    INDEX = unlist(sapply(charCountList, function(x) names(x))),
    FUN = sum
  )
  if(is.null(regexCharsToKeep)){
    charCountFiltered <- charCount
  } else {
    charCountFiltered <- charCount[grep(regexCharsToKeep, names(charCount))]
  }
  charCountFiltered[order(charCountFiltered, decreasing = decreasing)]
}