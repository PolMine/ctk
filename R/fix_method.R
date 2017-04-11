.repairVrtFile <- function(filename, sourceDir=NULL, targetDir=NULL, verbose=FALSE, param=list()){
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
      file.path(sourceDir, filename), sep="\n", what="character",
      blank.lines.skip=TRUE, quiet=ifelse(verbose==TRUE, FALSE, TRUE),
      fileEncoding=fileEncoding
      
      )    
  }
  vrt <- .repairVrtCharacterVector(vrt, verbose=verbose)
  if (!is.null(targetDir)){
    if (verbose == TRUE) message("... writing ", filename)
    if (length(grep('^<.*?>$', vrt)) != length(vrt)){
      cat(vrt, file=file.path(targetDir, filename), sep="\n")  
    } else {
      if (verbose == TRUE) message("... file without text - skipping ", filename)
    }
    return(Sys.time() - startTime)
  } else {
    return(paste(vrt, collapse="\n"))
  } 
}

.repairVrtCharacterVector <- function(vrt, verbose){
  if (verbose == TRUE) message("... removing leading and trailing spaces")
  vrt2 <- sapply(vrt, function(x) gsub("^\\s*(.*?)\\s*$", "\\1", x), USE.NAMES=FALSE)
  if (verbose == TRUE) message("... repair buggy lines")
  vrt3 <- unlist(lapply(vrt2, function(x) {
    if (grepl("#unknown#\t#unknown#", x)){
      retval <- c(
        gsub("^(.*?#unknown#)\t.*$", "\\1", x, perl=TRUE),
        gsub("^.*\t#unknown#\t(#unknown#.*?)$", "\\1", x, perl=TRUE)
      )
    } else {
      retval <- x
    }
    retval
  }))
  for (x in grep("#unknown#</p>", vrt3)){
    vrt3[x-1] <- paste(vrt3[x-1], "\t#unknown#", sep="")
    vrt3[x] <- "</p>"
  }
  if (verbose == TRUE) message("... remove empty tags")
  selfClosingTags <- grep("^\\s*<.*?/>\\s*$", vrt3)
  misleadingTags <- grep('^[<>]\\s+.*$', vrt3)
  indexToRemove <- c(selfClosingTags, misleadingTags)
  vrt4List <- as.list(vrt3)
  for (i in rev(indexToRemove)) vrt4List[[i]] <- NULL
  vrt4 <- unlist(vrt4List)
  vrt5 <- gsub(" ", " ", vrt4)
  vrt6 <- gsub("<unknown>", "#unknown#", vrt5)
  vrt7 <- gsub("&", "&amp;", vrt6)
  return(vrt7)
}

#' correct/polish vrt files
#' 
#' The treetagger wrapper script results in some faulty lines - this is corrected here.
#' 
#' @param x a character vector providing a directory with vrt files
#' @param targetDir a character vector
#' @param mc whether to use multicore
#' @param verbose logicel, defaults to TRUE
#' @exportMethod fix
#' @aliases fix-method fix
#' @rdname fix-method
#' @rdname pipe
setMethod("fix", "pipe", function(x, sourceDir, targetDir, encoding="UTF-8", ...){
  checkDirs(x, sourceDir, targetDir)
  dirApply(
    f=.repairVrtFile,
    sourceDir=file.path(x@projectDir, sourceDir),
    targetDir=file.path(x@projectDir, targetDir),
    param=list(encoding=encoding),
    ...
    )
})

setMethod("fix", "character", function(x){
  .repairVrtFile(x)
})

