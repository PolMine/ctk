#' apply named entity recognition to files in a dir (using StanfordNLP)
#' 
#' @param .Object a ctk object
#' @param sourceDir the source directory
#' @param targetDir the target directory
#' @param pattern process only files matching the pattern
#' @param mc use multicore
#' @param progress show progress bar
#' @param verbose whether to be verbose
#' @param sample process only a subset
#' @param files process only the files specified
#' @param continue process only files not yet present in target dir
#' @param failsafe proceed in a failsafe manner
#' @param ... further paramters
#' @rdname ner-method
#' @name ner
#' @exportMethod tokenize
setGeneric("ner", function(.Object, ...) standardGeneric("ner"))


.nerWorker <- function(filename, sourceDir, targetDir, ...){
  pathStanfordNer <- "/opt/stanfordNLP/stanford-ner-2015-01-30/stanford-ner.jar"
  # classifier <- "/opt/stanfordNLP/stanford-ner-2015-01-30/classifiers/english.nowiki.3class.distsim.crf.ser.gz"
  # classifier <- "/opt/stanfordNLP/stanford-ner-2015-01-30/classifiers/hgc_175m_600.crf.ser.gz"
  classifier <- "/opt/stanfordNLP/stanford-ner-2015-01-30/classifiers/dewac_175m_600.crf.ser.gz"
  cmdVector <- c(
    "/opt/jdk1.8.0_45/bin/java", "-cp", pathStanfordNer, "-mx600m",
    "edu.stanford.nlp.ie.crf.CRFClassifier",
    "-loadClassifier", 
    classifier,
    "edu.stanford.nlp.process.PTBTokenizer",
    "-textFile",
    file.path(sourceDir, filename), ">",
    file.path(targetDir, gsub("\\.tok$", ".ner", filename))
    )  
  cmd <- paste(cmdVector, collapse=" ")
  if (verbose == TRUE) message("... processing ", filename)
  system(cmd, intern=TRUE)
}

#' @rdname ner-method
#' @examples
#' \dontrun{
#' bt <- plprbtpdf("/home/blaette/plprbtpdf")
#' tokenize(bt, sourceDir="xmlCwb", targetDir="tmp")
#' }
setMethod("ner", "ctkPipe", function(
  .Object, sourceDir, targetDir,
  pattern="xml", mc=FALSE, progress=TRUE, verbose=FALSE, sample=FALSE, files=NULL, continue=FALSE, failsafe=FALSE, ...
  ){
  checkDirs(.Object=.Object, sourceDir, targetDir)
  dirApply(
    f=.nerWorker, x=.Object, sourceDir=sourceDir, targetDir=targetDir,
    pattern=pattern, mc=mc, progress=progress, verbose=verbose,
    sample=sample, files=files, continue=continue, failsafe=failsafe, ...       
    )
})