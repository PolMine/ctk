.repairVrtFile <- function(filename, sourceDir = NULL, targetDir = NULL, verbose = FALSE, param = list()){
  if (!is.null(targetDir)) startTime <- Sys.time()
  if ("encoding" %in% names(param)){
    fileEncoding <- param[["encoding"]]
  } else {
    fileEncoding <- "UTF-8"
  }
  if (is.null(sourceDir)){
    vrt <- strsplit(filename, "\n")[[1]]
  } else {
    vrt <- scan(
      file.path(sourceDir, filename), sep = "\n", what = "character",
      blank.lines.skip = TRUE, quiet = !verbose,
      fileEncoding = fileEncoding
    )    
  }
  vrt <- .repairVrtCharacterVector(vrt, replacements = param[["replacements"]], verbose = verbose)
  if (!is.null(targetDir)){
    if (verbose) message("... writing ", filename)
    if (length(grep('^<.*?>$', vrt)) != length(vrt)){
      cat(vrt, file = file.path(targetDir, filename), sep = "\n")  
    } else {
      warning("... no text in file, skipping: ", filename)
    }
    return(Sys.time() - startTime)
  } else {
    return(paste(vrt, collapse = "\n"))
  } 
}

.repairVrtCharacterVector <- function(vrt, replacements, verbose){
  
  # remove leading and trailing spaces
  vrt2 <- gsub("^\\s*(.*?)\\s*$", "\\1", vrt)
  # repair buggy lines
  
  vrt3 <- unlist(lapply(
    vrt2,
    function(x) {
      if (grepl("#unknown#\t#unknown#", x)){
        retval <- c(
          gsub("^(.*?#unknown#)\t.*$", "\\1", x, perl = TRUE),
          gsub("^.*\t#unknown#\t(#unknown#.*?)$", "\\1", x, perl = TRUE)
        )
      } else {
        retval <- x
      }
      retval
    }))
  
  # misplaced closing tags (at end of line)
  for (i in grep("^.+?</.*?>$", vrt3, perl = T)){
    if (grepl("^.*?\\s.*?\\s.*?</.*?>$", vrt3[i])){
      # a line such as: 'professionell	ADJD	professionell</p>' / 'oder	KON	od</p>'
      vrt3[i] <- gsub("^(.*?\\s.*?\\s.*?)(</.*?>)$", "\\1\n\\2", vrt3[i])
    } else {
      # a line such as 'kommun</p>' or 'Dezember</p>'
      vrt3[i] <- gsub("^.*?(</.*?>)$", "\\1", vrt3[i])
    }
  }
  
  # repair wrong position of closing tags
  for (x in grep("#unknown#</p>", vrt3)){
    vrt3[x-1] <- paste(vrt3[x-1], "\t#unknown#", sep = "")
    vrt3[x] <- "</p>"
  }
  
  # remove empty tags
  selfClosingTags <- grep("^\\s*<.*?/>\\s*$", vrt3)
  misleadingTags <- grep('^[<>]\\s+.*$', vrt3)
  indexToRemove <- c(selfClosingTags, misleadingTags)
  vrt4List <- as.list(vrt3)
  for (i in rev(indexToRemove)) vrt4List[[i]] <- NULL
  vrt4 <- unlist(vrt4List)
  
  # remove characters incompatible with XML
  vrt5 <- gsub(" ", " ", vrt4)
  vrt6 <- gsub("<unknown>", "#unknown#", vrt5)
  vrt7 <- gsub("&", "&amp;", vrt6)
  vrt7 <- gsub("\xC2\xA0", "", vrt7)
  
  # anything else
  vrt8 <- gsub("^„\\t[A-Z]+\\t#unknown#$", "'\t$(\t'", vrt7) # 
  vrt9 <- gsub("^``\\t.*?\\t``$", "'\t$(\t'", vrt8)
  vrt10 <- gsub('^\\s*<\\s*$', "", vrt9)
  vrt10 <- gsub("^(<.*?>)(<.*?>)$", "\\1\n\\2", vrt10) # two tags that happen to be in the same line
  
  # apply user-defined replacements
  for (i in 1:length(replacements)){
    vrt10 <- gsub(replacements[[i]][1], replacements[[i]][2], vrt10)
  }
  
  emptyLines <- grep("^\\s*$", vrt10)
  if (length(emptyLines) > 0) vrt10 <- vrt10[-emptyLines]
  
  return(vrt10)
}

#' Fix vrt files for CWB import.
#' 
#' Files resulting from tagging/annotation may violate the requirements of the 
#' Corpus Workbench (CWB). The \code{fix} method repairs the known issues
#' the vrt files may cause.
#' 
#' Known issues resulting from annotating files (with the treetagger in particular)
#' are whitespace characters invalid for XML, XML elements at the end of a line
#' rather than in a seperate line, characters invalid for XML (such as ampersands),
#' inter alia.
#' 
#' Before doing respective corrections, the method tests whether there is any text at
#' all in the files. Empty files (files that contain nothing but XML tags) are dropped.
#' 
#' @param x a character vector providing a directory with vrt files
#' @param sourceDir character vector with directory with files
#' @param targetDir a character vector
#' @param encoding encoding of the file, used by scan
#' @param replacements a list of character vectors (length 2 each) with regular expressions / replacements
#' @param mc whether to use multicore
#' @param verbose logical, defaults to TRUE
#' @exportMethod fix
#' @aliases fix-method fix
#' @rdname fix-method
#' @rdname pipe
setMethod("fix", "pipe", function(x, sourceDir, targetDir, encoding = "UTF-8", replacements = list(), ...){
  checkDirs(x, sourceDir, targetDir)
  dirApply(
    f = .repairVrtFile,
    sourceDir = file.path(x@projectDir, sourceDir),
    targetDir = file.path(x@projectDir, targetDir),
    param = list(encoding = encoding, replacements = replacements),
    ...
    )
})

setMethod("fix", "character", function(x){
  .repairVrtFile(x)
})
