.consolidate <- function(filename, sourceDir = NULL, targetDir = NULL, verbose = FALSE, param = list()){
  if (!is.null(targetDir)) startTime <- Sys.time()
  fileEncoding <- if ("encoding" %in% names(param)) param[["encoding"]] else "UTF-8"
  vrt <- if (is.null(sourceDir)) strsplit(filename, "\n")[[1]] else readLines(file.path(sourceDir, filename))
  vrt <- .repair(vrt, replacements = param[["replacements"]], verbose = verbose)
  if (!is.null(targetDir)){
    if (verbose) message("... writing ", filename)
    if (all(grepl('^<.*?>$', vrt))){
      warning("... no text in file, skipping: ", filename)
    } else {
      cat(vrt, file = file.path(targetDir, filename), sep = "\n")  
    }
    return( Sys.time() - startTime )
  } else {
    return(paste(vrt, collapse = "\n"))
  } 
}

.repair <- function(vrt, replacements, verbose){
  
  # remove leading and trailing spaces
  vrt2 <- gsub("^\\s*(.*?)\\s*$", "\\1", vrt)
  
  # repair buggy lines
  vrt3 <- ifelse(
    grepl("#unknown#\t#unknown#", vrt2),
    c(
      gsub("^(.*?#unknown#)\t.*$", "\\1", vrt2, perl = TRUE),
      gsub("^.*\t#unknown#\t(#unknown#.*?)$", "\\1", vrt2, perl = TRUE)
    ),
    y
  )

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
  indexToRemove <- unique(c(selfClosingTags, misleadingTags))
  y <- if (length(indexToRemove) > 0) vrt3[-indexToRemove] else vrt3

  # run through replacements
  knownBugs <- c(
    c("\u00A0", " "), # incompatible with XML
    c("<unknown>", "#unknown#"), # incompatible with XML
    c("&", "&amp;"), # incompatible with XML
    c("\xC2\xA0", ""), # incompatible with XML
    c("^\u201E\\t[A-Z]+\\t#unknown#$", "'\t$(\t'"),
    c("^``\\t.*?\\t``$", "'\t$(\t'"),
    c('^\\s*<\\s*$', ""),
    c("^(<.*?>)(<.*?>)$", "\\1\n\\2")
  )
  toReplace <- c(knownBugs, replacements)
  for (i in 1:length(toReplace)) y <- gsub(toReplace[[i]][1], toReplace[[i]][2], y)

  emptyLines <- grep("^\\s*$", y)
  if (length(emptyLines) > 0) y <- y[-emptyLines]
  
  return(y)
}

#' Consolidate vrt files for CWB import.
#' 
#' Files resulting from tagging/annotation may violate the requirements of the 
#' Corpus Workbench (CWB).  Consolidate the known issues the vrt files may cause.
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
#' @param ... further parameters passed into \code{dirApply}
#' @export consolidate
#' @rdname consolidate
consolidate <- function(x, sourceDir, targetDir, encoding, replacements, ...){
  .consolidate(filename = x, sourceDir, targetDir, param = list(encoding = encoding, replacements = replacements))
}
