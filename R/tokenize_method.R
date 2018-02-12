#' Tokenize files.
#' 
#' Tokenize (XML) files with one standard tool (treetagger, stanfordNLP, openNLP).
#' 
#' One potential problem with the perl-tokenizer that comes with the treetagger
#' is that the output is not valid XML. It is necessary to fix the XML with a 
#' shell command such as \code{for i in $(ls); do sed 's/\xC2\xA0/ /g' $i > ../tok2/$i; done}.
#' The XML may still not be valid ("&" etc.), so fix method is still necessary.
#' 
#' @param .Object a ctk object
#' @param with either "stanfordNLP", "treetagger" or "openNLP"
#' @param lang language of the files to be tagged
#' @param ... further paramters
#' @rdname tokenize
#' @name tokenize
#' @exportMethod tokenize
setGeneric("tokenize", function(.Object, ...) standardGeneric("tokenize"))


.tokenizeWorker <- function(filename, sourceDir = NULL, targetDir = NULL, verbose = FALSE, param = list()){
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
    tmpFilename <- tempfile(pattern=tmpFilenamePattern, tmpdir=sourceDir, fileext=".xml")
    tmpFilename <- strsplit(tmpFilename, "/")[[1]][length(strsplit(tmpFilename, "/")[[1]])]
    cat(filename, file=file.path(sourceDir, tmpFilename), sep="\n")
    filename <- tmpFilename
  }
  with <- param[["with"]]
  lang <- param[["lang"]]
  if (with == "stanfordNLP"){
    pathStanfordNlp <- "/opt/stanfordNLP/stanford-corenlp-full-2015-04-20/stanford-corenlp-3.5.2.jar"
    pathStanfordNer <- "/opt/stanfordNLP/stanford-ner-2015-01-30/stanford-ner.jar"
    if (lang %in% c("de", "en")){
      tokenizerClass <- "edu.stanford.nlp.process.PTBTokenizer"
      cmdVector <- c(
        "/opt/jdk1.8.0_45/bin/java", "-cp", pathStanfordNlp,
        tokenizerClass,
        file.path(sourceDir, filename), ">",
        file.path(targetDir, gsub("\\.xml$", ".tok", filename))
      )
    } else if (lang == "fr"){
      cmdVector <- c(
        "/opt/jdk1.8.0_45/bin/java", "-cp",
        paste(
          system.file("java", package="ctk"),
          pathStanfordNlp, sep=":"
          ),
        "MyFrenchTokenizer",
        file.path(sourceDir, filename), ">",
        file.path(targetDir, gsub("\\.xml$", ".tok", filename))
        )
    }
    cmd <- paste(cmdVector, collapse=" ")
    # system(cmd, intern=TRUE, ignore.stdout=TRUE, ignore.stderr=TRUE)
    system(cmd, intern=TRUE, ignore.stderr=TRUE)
  } else if (with == "treetagger"){
    langList <- list("en"="-e", "fr"="-f", "it"="-i", "de"="")
    stopifnot(lang %in% names(langList))
    cmdVector <- c(
      "perl", "/opt/treetagger/cmd/utf8-tokenize.perl",
      langList[[lang]],
      file.path(sourceDir, filename), ">",
      file.path(targetDir, gsub("\\.xml$", ".tok", filename))  
      )
    cmd <- paste(cmdVector, collapse=" ")
    system(cmd, intern=TRUE)
  } else if (with == "openNLP"){
    stop("using openNLP is not supported any more")
  }
  if (returnString == TRUE){
    retval <- scan(file.path(targetDir, gsub("^(.*?)\\.xml$", "\\1.tok", filename)), what="character", sep="\n", quiet=TRUE)
    retval <- paste(retval, collapse="\n")
    file.remove(file.path(targetDir, filename))
  } else {
    retval <- Sys.time() - startTime
  }
  retval
}

#' @rdname tokenize
setMethod("tokenize", "character", function(.Object, lang = "de", with="stanfordNLP", ...){
  .tokenizeWorker(
    filename = .Object, sourceDir=NULL, targetDir=NULL,
    verbose=FALSE, param=list(lang=lang, with=with, ...)
    )
})

