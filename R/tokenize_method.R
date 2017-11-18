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

# .openNLPtokenizer <- function(filename, sourceDir, targetDir, startTime=Sys.time(), returnString=FALSE, lang="en"){
#   xmlDoc <- xmlTreeParse(file.path(sourceDir, filename), useInternalNodes=TRUE)
#   textNodes <- getNodeSet(xmlDoc, path="//p")
#   newTextNodes <- lapply(
#     c(1:length(textNodes)),
#     function(i) {
#       txt <- xmlValue(textNodes[[i]])
#       # anno <- NLP::annotate(txt, list(sentTokenAnnotator, wordTokenAnnotator))
#       sentTokenAnnotator <- Maxent_Sent_Token_Annotator()
#       annotationSentences <- NLP::annotate(txt, sentTokenAnnotator)
#       wordTokenAnnotator <- openNLP::Maxent_Word_Token_Annotator(language=lang)
#       anno <- NLP::annotate(txt, wordTokenAnnotator, annotationSentences)
#       sentenceAnno <- anno[which(sapply(anno, function(x) x$type) == "sentence")]
#       sentencesTokenized <- lapply(
#         sentenceAnno,
#         function(sAnno) {
#           constituents <- sAnno$features[[1]]$constituents
#           sapply(constituents, function(x) {substr(txt, start=anno[x]$start, stop=anno[x]$end)} )
#         })
#       txtTagged <- sapply(
#         sentencesTokenized,
#         function(tokenVector){paste("<s>\n", paste(tokenVector, collapse="\n"), "\n</s>", sep="")}
#       )
#       # txtTagged <- paste(sentencesTagged, collapse="\n")
#       gsub("&", "&amp;", txtTagged)
#     }
#   )
#   # rm(sentTokenAnnotator)
#   # rm(wordTokenAnnotator)
#   oldTextNodes <- lapply(
#     c(1:length(textNodes)),
#     function(i){
#       # parentOfTextNode <- getNodeSet(textNodes[[i]], path="..")[[1]]
#       # removeNodes(textNodes[[i]])
#       # addChildren(parentOfTextNode, getNodeSet(xmlParse(newTextNodes[[i]]), path="/p/s"))
#       removeNodes(getNodeSet(textNodes[[i]], path="./text()"))
#       addChildren(textNodes[[i]], lapply(newTextNodes[[i]], function(x) xmlRoot(xmlParse(x))))
# #       replaceNodes(
# #         oldNode=textNodes[[i]],
# #         newNode=lapply(newTextNodes[[i]], function(x) xmlRoot(xmlParse(x)))
# #       )
#     }
#   )
#   tokVectorRaw <- saveXML(
#     xmlDoc, prefix='<?xml version="1.0" encoding="UTF-8"?>\n',
#     file=NULL, indent=TRUE, encoding="UTF-8"
#   )
#   tokVector <- gsub("^\\s*", "", strsplit(tokVectorRaw, "\n")[[1]])
#   if (returnString == FALSE){
#     tokFilename <- gsub("^(.*?)\\.xml$", "\\1.tok", filename)
#     cat(tokVector, file=file.path(targetDir, tokFilename), sep="\n")
#     return(Sys.time() - startTime)
#   } else {
#     return(tokVector)
#   }
# }


.tokenizeWorker <- function(filename, sourceDir=NULL, targetDir=NULL, verbose=FALSE, param=list()){
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
    if (
      requireNamespace("openNLP", quietly=TRUE)
      && requireNamespace("NLP", quietly=TRUE)
      ){
      # sourceDir <- "/home/blaette/Lab/repos/keywords/data/repubblica/xml"
      # targetDir <- "/home/blaette/Lab/repos/keywords/data/repubblica/tok"
      # lang <- "it"
      # sentTokenAnnotator <- openNLP::Maxent_Sent_Token_Annotator(language=lang)
      # wordTokenAnnotator <- openNLP::Maxent_Word_Token_Annotator(language=lang)
      # 
      # foo <- list.files(sourceDir)
      # i <- 1
      # filename <- foo[i]
      # .openNLPtokenizer(filename=foo[i], sourceDir=sourceDir, targetDir=targetDir, lang=lang)
      return(
        .openNLPtokenizer(filename=filename, sourceDir=sourceDir, targetDir=targetDir, startTime=startTime, returnString=returnString, lang=lang)
        )
    } else {
      warning("package 'openNLP' is not installed")
      stop()
    } 
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

