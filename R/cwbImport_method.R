#' @include ctkPipe_class.R
NULL


setGeneric("cwbImport", function(.Object, ...) standardGeneric("cwbImport"))

#' import into CWB
#' 
#' This is a wrapper for the cwb-encode and cwb-make command line tools.
#' 
#' @param .Object a character vector
#' @param corpus the corpus name
#' @param cwbRegistry the directory of the CWB registry
#' @param sAttributes a list, use sAttributes-method
#' @param xml whether files are XML
#' @param verbose defaults to TRUE
#' @rdname cwbImport-method
#' @exportMethod cwbImport
setMethod("cwbImport", "character", function(.Object, corpus="FOO", cwbRegistry, sAttributes, xml=TRUE, verbose=TRUE){
  tmp <- unlist(strsplit(cwbRegistry, "/"))
  cwbDirs <- list.dirs(paste("/", paste(tmp[2:(length(tmp)-1)], collapse="/"), sep=""), recursive=FALSE)
  indexedCorpusDir <- cwbDirs[grep("indexed", cwbDirs)]
  if (tolower(corpus) %in% list.dirs(indexedCorpusDir, full.names=FALSE, recursive=FALSE)){
    message("There is already a directory for the corpus. Continue anyway (yes/no)?")
    feedback <- readline()
    if (!feedback == "yes") stop("cwb encode cancelled")
  } else {
    dir.create(paste(indexedCorpusDir, "/", tolower(corpus), sep=""))
  }
  sAttrCmd <- vapply(
    names(sAttributes),
    FUN.VALUE="character", USE.NAMES=FALSE,
    function(sAttr) {
      ret <- paste("-S ", sAttr, sep="")
      if (!is.na(sAttributes[[sAttr]][1])){
        attributes <- paste(sAttributes[[sAttr]], collapse="+")      
        ret <- paste(ret, ":0+", attributes, sep="")
      }
      ret
    }
  )
  cmd <- c(
    "cwb-encode",
    "-d", file.path(indexedCorpusDir, tolower(corpus)),
    "-F", file.path(.Object),
    "-R", file.path(cwbRegistry, tolower(corpus)),
    "-P", "pos",
    "-P", "lemma",
    sAttrCmd
  )
  if (xml == TRUE) cmd <- c(cmd, "-xsB")
  cmd <- paste(cmd, collapse=" ")
  if (verbose == TRUE) message("... encoding the CWB corpus")
  if (verbose == TRUE) print(cmd)
  ret <- system(cmd)
  if (ret == 0) {
    if (verbose == TRUE) message("... cwb-make")
    cwbMakeCmd <- paste("cwb-make -V", toupper(corpus), "-r", cwbRegistry)
    print(cwbMakeCmd)
    system(cwbMakeCmd)
  } else {
    stop("some problem with cwb-encode")
  }
  message("... corpus setup appears to be successful")
  return(TRUE)
})


#' @param corpus CWB corpus name that shall be created
#' @param xml logical
#' @exportMethod cwbImport
#' @rdname ctkPipe
setMethod("cwbImport", "ctkPipe", function(.Object, corpus, sourceDir, xml=TRUE, verbose=TRUE, ...){
  if (length(.Object@sAttributes) == 0) {
    if (verbose == TRUE) message("... getting sAttributes")
    .Object <- sAttributeList(.Object, verbose=verbose, ...) 
  }
  cwbImport(.Object=file.path(.Object@projectDir, sourceDir), corpus=corpus, cwbRegistry=.Object@cwbRegistry, sAttributes=.Object@sAttributes, xml=xml)
  return(.Object)
})