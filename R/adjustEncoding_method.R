#' @include ctkPipe_class.R
NULL

#' apply iconv to files in a directory
#' 
#' @param .Object a character string providing a directory
#' @param targetDir where to put files
#' @param from encoding of the files in the the sourceDir, if NULL, the command line file utility will be used to detect the encoding of the source file
#' @param to final encoding
#' @param mc use multicore
#' @param progress whether to display progress bar
#' @return something
#' @rdname adjustEncoding-method
#' @exportMethod adjustEncoding
#' @importFrom stringi stri_enc_detect
setGeneric("adjustEncoding", function(.Object, ...) standardGeneric("adjustEncoding"))


.iconvWorker <- function(filename, sourceDir, targetDir=NULL, verbose=FALSE, param=list()) {
  startTime <- Sys.time()
  if (is.null(targetDir)) targetDir <- sourceDir
  from <- param[["from"]]
  to <- param[["to"]]
  isXml <- param[["xml"]]
  availableEncodings <- unlist(strsplit(gsub("//", "", system("iconv -l", intern=TRUE)), " "))
  if (is.null(from)){
    fileRetval <- system(paste("file", "--mime", file.path(.Object, filename), collapse=" "), intern=TRUE)
    fileEncoding <- gsub("^.*charset=(.*)$", "\\1", fileRetval)
    if (fileEncoding %in% c("binary", "unknown-8bit")){
      content <- paste(scan(file.path(.Object, filename), what="character", quiet=TRUE), sep="\n", collapse="\n")
      fileEncoding <- stri_enc_detect(content)[[1]][["Encoding"]][1]
    }
    from <- toupper(fileEncoding)
  }
  if (!from %in% availableEncodings) warning("Encoding ", from, " is not an encoding that can be processed by iconv")
  if (!to %in% availableEncodings) warning("Encoding ", to, " is not an encoding that can be processed by iconv")
  cmdIconvRaw <- c(
    "iconv", "-f", from, "-t", to, "-c",
    '--unicode-subst=" "',
    file.path(sourceDir, filename),
    ">", file.path(targetDir, filename)
  )
  cmdIconv <- paste(cmdIconvRaw, collapse=" ")
  system(cmdIconv)
  if (isXml == TRUE){
    cmdSedRaw <- c(
      "sed", "-i", "''",
      paste("'1s/", from, "/", to, "/'", sep=""),
      file.path(targetDir, filename)
    )
    cmdSed <- paste(cmdSedRaw, collapse=" ")
    system(cmdSed)
  }
  return(Sys.time()-startTime)
}


#' @rdname adjustEncoding-method
setMethod("adjustEncoding", "character", function(.Object, targetDir=NULL, from=NULL, to, xml=FALSE, ...){  
  availableEncodings <- gsub("//", "", system("iconv -l", intern=TRUE))
  dirApply(
    f=.iconvWorker,
    sourceDir=.Object, targetDir=targetDir,
    param=list(from=from, to=to, xml=xml),
    ...
  )
})

#' @rdname adjustEncoding-method
setMethod("adjustEncoding", "ctkPipe", function(.Object, sourceDir, targetDir, from="UTF-8", to="ISO-8859-1", xml=FALSE, ...){
  checkDirs(.Object, sourceDir, targetDir)
  dirApply(
    f=.iconvWorker,
    sourceDir=file.path(.Object@projectDir, sourceDir),
    targetDir=file.path(.Object@projectDir, targetDir),
    param=list(from=from, to=to, xml=xml),
    ...
  )
  
})



