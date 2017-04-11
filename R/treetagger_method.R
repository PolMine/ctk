#' @include pipe_class.R
NULL


setGeneric("treetagger", function(.Object, ...) standardGeneric("treetagger"))


#' use TreeTagger
#' 
#' @param .Object a path
#' @param targetDir output directory
#' @param treetaggerPath directory with the treetagger
#' @param parallel whether to use parallel computing
#' @param lang the language to use
#' @param verbose logical, defaults to TRUE
#' @rdname treetagger-method
#' @exportMethod treetagger

.treetaggerWorker <- function(filename, sourceDir=NULL, targetDir=NULL, verbose=FALSE, param=list()){
  if (is.null(targetDir)){
    targetDir <- tempdir()
    returnString <- TRUE
  } else {
    returnString <- FALSE
    startTime <- Sys.time()
  }
  if (is.null(sourceDir)){
    sourceDir <- ifelse(is.null(targetDir), tempdir(), targetDir)
    tmpFilenamePattern <- ifelse("thread" %in% names(param), param[["thread"]], "thread1")
    tmpFilename <- tempfile(pattern=tmpFilenamePattern, tmpdir=sourceDir, fileext=".tok")
    tmpFilename <- strsplit(tmpFilename, "/")[[1]][length(strsplit(tmpFilename, "/")[[1]])]
    cat(filename, file=file.path(sourceDir, tmpFilename), sep="\n")
    filename <- tmpFilename
  }
  
  lang <- param[["lang"]]
  filenameOut <- gsub("^(.*)\\..*?$", "\\1.vrt", filename)
  if (lang == "de"){
    parFile <- file.path(Sys.getenv("PATH_TREETAGGER"), "lib", "german-utf8.par")
  } else if (lang == "fr"){
    parFile <- file.path(Sys.getenv("PATH_TREETAGGER"), "lib", "french-utf8.par")
  } else if (lang == "it"){
    parFile <- file.path(Sys.getenv("PATH_TREETAGGER"), "lib", "italian-utf8.par")
  } else if (lang == "en"){
    parFile <- file.path(Sys.getenv("PATH_TREETAGGER"), "lib", "english-utf8.par")
  } else {
    warning("the language is (not yet) supported")
  }
  cmdRaw <- c(
    "/opt/treetagger/bin/tree-tagger", "-sgml",
    "-token", "-lemma", parFile,
    file.path(sourceDir, filename),
    file.path(targetDir, filenameOut)
    )
  cmd <- paste(cmdRaw, collapse=" ")
  if (verbose == TRUE) print(cmd)
  capture.output(system(cmd, intern=TRUE, ignore.stdout=TRUE, ignore.stderr=TRUE))
  
  if (returnString == TRUE){
    retval <- scan(file.path(targetDir, gsub("^(.*?)\\.tok$", "\\1.vrt", filename)), what="character", sep="\n", quiet=TRUE)
    retval <- paste(retval, collapse="\n")
    file.remove(file.path(targetDir, filename))
  } else {
    retval <- Sys.time() - startTime
  }
  retval
}


#' tag one or multiple files
#' 
#' Convert XML input files into a tagged corpus
#' 
#' @param lang the language to be used (defaults to 'de')
#' @return the verbose output of the tagging script that is called
#' @exportMethod treetagger
#' @rdname pipe
setMethod("treetagger", "pipe", function(
  .Object,
  sourceDir="xml",  targetDir="vrt", py=FALSE, lang="de",
  ...
){
  checkDirs(.Object, sourceDir, targetDir)
  if (py==TRUE){
    treetagger(
      .Object=file.path(.Object@projectDir, sourceDir),
      targetDir=file.path(.Object@projectDir, targetDir),
      treetaggerPath=.Object@treetaggerPath,
      parallel=mc, lang=lang, verbose=verbose
    )    
    retval <- NULL
  } else {
    retval <- dirApply(
      f=.treetaggerWorker,
      sourceDir=file.path(.Object@projectDir, sourceDir), targetDir=file.path(.Object@projectDir, targetDir),
      param=list(lang=lang), ...
      )
  }
  retval
})

setMethod("treetagger", "character", function(.Object, lang="de", fix = TRUE){
  vrtRaw <- .treetaggerWorker(filename = .Object, param = list(lang=lang))
  fixVrt(vrtRaw)
})
