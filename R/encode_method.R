#' @rdname encode-method
setGeneric("encode", function(.Object, ...) standardGeneric("encode"))

#' Encode corpus (CWB import).
#' 
#' This is a wrapper for the \code{cwb-encode} and \code{cwb-make} command line tools. 
#' The calls are generated and executed (via a system call).
#' 
#' @param .Object a character vector
#' @param corpus the corpus name
#' @param registry the directory of the CWB registry
#' @param sAttributes a list, use sAttributes-method
#' @param xml whether files are XML
#' @param exec logical, whether to execute commands
#' @param embedding levels of embedding elements to allow for 
#' @param verbose defaults to TRUE
#' @param encoding encoding, passed into \code{cwb-encode}
#' @param ... further parameters (not passed)
#' @rdname encode-method
#' @exportMethod encode
#' @importClassesFrom polmineR regions
#' @importFrom polmineR size getTokenStream
setMethod("encode", "character", function(.Object, corpus = "FOO", registry = Sys.getenv("CORPUS_REGISTRY"), sAttributes, embedding = "0", encoding = "utf8", xml = TRUE, exec = TRUE, verbose = TRUE, ...){
  tmp <- unlist(strsplit(registry, "/"))
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
    FUN.VALUE = "character", USE.NAMES = FALSE,
    function(sAttr) {
      ret <- paste("-S ", sAttr, sep = "")
      if (!is.na(sAttributes[[sAttr]][1])){
        attributes <- paste(sAttributes[[sAttr]], collapse = "+")      
        ret <- paste(ret, ":", embedding, "+", attributes, sep = "")
      }
      ret
    }
  )
  cmd <- c(
    "cwb-encode",
    "-d", file.path(indexedCorpusDir, tolower(corpus)),
    "-F", .Object,
    "-R", file.path(registry, tolower(corpus)),
    "-P", "pos",
    "-P", "lemma",
    "-c", encoding,
    sAttrCmd
  )
  if (xml == TRUE) cmd <- c(cmd, "-xsB")
  cmd <- paste(cmd, collapse=" ")
  if (verbose) message("... encoding the CWB corpus")
  if (verbose) print(cmd)
  if (exec){
    ret <- system(cmd)
  } else {
    ret <- 0
  }
  
  if (ret == 0) {
    if (verbose == TRUE) message("... cwb-make")
    cwbMakeCmd <- paste("cwb-make -V", toupper(corpus), "-r", registry)
    print(cwbMakeCmd)
    if (exec) system(cwbMakeCmd)
  } else {
    stop("some problem with cwb-encode")
  }
  message("... corpus setup appears to be successful")
  return(TRUE)
})




#' @param filename a filename
#' @rdname encode-method
#' @importFrom data.table data.table setorder setnames := setkeyv
setMethod("encode", "regions", function(.Object, filename = NULL, verbose = TRUE){
  cposDT <- .Object@cpos[!is.na(.Object@cpos[["cpos_right"]])]
  cposDT[, "index" := 1:nrow(cposDT), with = FALSE]
  setkeyv(cposDT, cols = c("cpos_right", "cpos_left"))
  .SD <- data.table() # not used, a dummy to pass tests
  .unfold <- function(.SD){
    left <- as.integer(.SD[1, "cpos_left", with = FALSE])
    right <- as.integer(.SD[1, "cpos_right", with = FALSE])
    data.table(cpos = left:right, id = .SD[["id"]])
  }
  
  if (verbose) message("... unfolding regions to annotated tokens")
  cposDTextensive <- cposDT[, .unfold(.SD), by = "index", with = TRUE]
  
  if (verbose) message("... compressing annotations")
  .compress <- function(.SD) paste("|", paste(.SD[["id"]], collapse = "|"), "|", sep = "")
  cposDTaggr <- cposDTextensive[, .compress(.SD), by = "cpos"]
  setorder(cposDTaggr, cols = "cpos")
  
  if (verbose) message("... getting token stream")
  tokenStreamDT <- data.table(
    cpos = 0:(size(.Object@corpus) - 1),
    word = getTokenStream(.Object@corpus, pAttribute = "word")
  )
  
  if (verbose) message("... combining token stream and annotations")
  setkeyv(tokenStreamDT, cols = "cpos")
  setkeyv(cposDTaggr, cols = "cpos")
  tokenDT <- cposDTaggr[tokenStreamDT]
  setnames(tokenDT, old = "V1", new = "annotation")
  
  if (verbose) message("... preparing xml annotations")
  tokenDT[, "xml_left" := ifelse(is.na(tokenDT[["annotation"]]), "", paste('<annotation tag="', tokenDT[["annotation"]], '">\n', sep = "")), with = FALSE]
  tokenDT[, "word" := paste(tokenDT[["word"]], "\n", sep = ""), with = FALSE]
  tokenDT[, "xml_right" := ifelse(is.na(tokenDT[["annotation"]]), "", "</annotation>\n"), with = FALSE]
  
  if (verbose) message("... preparing output string")
  pasted <- paste(tokenDT[["xml_left"]], tokenDT[["word"]], tokenDT[["xml_right"]], sep = "")
  toWrite <- paste(pasted, collapse = "")
  toWrite <- paste('<?xml version = "1.0">\n', '<doc>\n', toWrite, "</doc>\n", sep = "")
  
  if (verbose) message("... writing file")
  if (is.null(filename)) filename <- tempfile()
  cat(toWrite, file = filename)
  # grep -v "^<" annotations.xml | wc -l
  
  if (verbose) message("... cwb-encode")
  cmdEncode <- c(
    "cwb-encode",
    "-d", "/Users/blaette/Lab/cwb/indexed_corpora/plprbt",
    "-f", filename,
    "-p -", "-0 doc", "-xsB",
    "-S annotation:0+tag"
  )
  system(paste(cmdEncode, sep = " ", collapse = " "))
  
  if (verbose) message("... cwb-regedit")
  cmdRegedit <- c("cwb-regedit", .Object@corpus, ":add", ":s", "annotation", "annotation_tag")
  system(paste(cmdRegedit, sep = " ", collapse = " "))
})

