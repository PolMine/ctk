#' Apply iconv to files in a directory.
#' 
#' @param .Object a character string providing a directory
#' @param targetDir where to put files
#' @param from encoding of the files in the the sourceDir, if NULL, the command line file utility will be used to detect the encoding of the source file
#' @param to final encoding
#' @param mc use multicore
#' @param progress whether to display progress bar
#' @return something
#' @rdname recode-method
#' @exportMethod recode
#' @importFrom stringi stri_enc_detect
setGeneric("recode", function(.Object, ... ) standardGeneric("recode"))



.getEncoding <- function(filename, sourceDir){
  fileRetval <- system(paste("file", "--mime", file.path(sourceDir, filename), collapse = " "), intern = TRUE)
  stringiEncoding <- NA
  guessedEncoding <- NA
  suggestedEncoding <- NA
  fileEncoding <- gsub("^.*charset=(.*)$", "\\1", fileRetval)
  if (fileEncoding %in% c("binary", "unknown-8bit")){
    content <- paste(scan(file.path(sourceDir, filename), what="character", quiet=TRUE, sep="\n"), collapse="\n")
    stringiEncoding <- stri_enc_detect(content)[[1]][["Encoding"]][1]
    Encoding(content) <- toupper(stringiEncoding)
    iconvTry <- iconv(content, from=toupper(stringiEncoding), to="UTF-8")
    if (is.na(iconvTry)){
      iconvTry2 <- iconv(content, from="CP850", to="UTF-8")
      if (!is.na(iconvTry2)){
        guessedEncoding <- "CP850"
        suggestedEncoding <- guessedEncoding
      }
    } else {
      suggestedEncoding <- stringiEncoding
    }
  } else {
    suggestedEncoding <- fileEncoding
  }
  encoding <- c(
    fileEncoding=fileEncoding, stringiEncoding=stringiEncoding,
    guessedEncoding=guessedEncoding, suggestedEncoding=suggestedEncoding
    )
  encoding <- toupper(encoding)
  encoding
}

.iconvWorker <- function(filename, sourceDir, targetDir = NULL, verbose = FALSE, param=list()) {
  log <- param[["log"]]
  startTime <- Sys.time()
  if (is.null(targetDir)) targetDir <- sourceDir
  from <- param[["from"]]
  to <- param[["to"]]
  isXml <- param[["xml"]]
  availableEncodings <- unlist(strsplit(gsub("//", "", system("iconv -l", intern=TRUE)), " "))
  if (is.null(from)) {
    encodingOptions <- .getEncoding(filename, sourceDir)
    if (log == TRUE) {
      cat(
        paste(filename, paste(encodingOptions, sep="\t", collapse="\t"), "\n", sep="\t"),
        file=file.path(targetDir, "encodingLogfile.log"), append=TRUE
        )
    }
    from <- encodingOptions["suggestedEncoding"]
  }
  if (!from %in% availableEncodings) warning("Encoding ", from, " is not an encoding that can be processed by iconv")
  if (!to %in% availableEncodings) warning("Encoding ", to, " is not an encoding that can be processed by iconv")
  cmdIconvRaw <- c(
    "iconv", "-f", from, "-t", to, "-c",
#    '--unicode-subst=" "',
    file.path(sourceDir, filename),
    ">", file.path(targetDir, filename)
  )
  cmdIconv <- paste(cmdIconvRaw, collapse=" ")
  system(cmdIconv)
  if (isXml == TRUE){
    cmdSedRaw <- c(
      "sed",
      paste("'1s/", from, "/", to, "/'", sep=""),
      file.path(targetDir, filename), ">",
      paste(file.path(targetDir, filename), "new", sep = ".")
    )
    cmdSed <- paste(cmdSedRaw, collapse=" ")
    system(cmdSed)
    file.remove(file.path(targetDir, filename))
    file.rename(
      from = paste(file.path(targetDir, filename), "new", sep = "."),
      to = file.path(targetDir, filename)
    )
  }
  return(Sys.time()-startTime)
}

#' @rdname recode-method
setMethod("recode", "character", function(.Object, targetDir=NULL, from=NULL, to, xml=FALSE, ...){  
  availableEncodings <- gsub("//", "", system("iconv -l", intern=TRUE))
  dirApply(
    f=.iconvWorker,
    sourceDir=.Object, targetDir=targetDir,
    param=list(from=from, to=to, xml=xml),
    ...
  )
})
