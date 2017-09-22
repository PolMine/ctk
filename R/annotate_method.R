setGeneric("annotate", function(.Object, ...) standardGeneric("annotate"))
  

setMethod("annotate", "character", function(.Object, targetDir, output = c("text", "xml", "json", "conll"), mc = 1){
  sourceDir <- .Object
  filesToProcess <- list.files(sourceDir, full.names = TRUE)
  filelistFile <- tempfile(fileext = ".txt")
  cat(filesToProcess, file = filelistFile, sep = "\n")
  
  cmdRaw <- c(
    "java",
    "-cp", paste('"', file.path(system.file(package = "coreNLP", "extdata", "stanford-corenlp-full-2015-12-09"), "*"), '"', sep = ""),
    "-Xmx4g",
    "edu.stanford.nlp.pipeline.StanfordCoreNLP",
    "-props", system.file(package = "coreNLP", "extdata", "StanfordCoreNLP-german.properties"),
    "-nthreads", mc,
    "-outputFormat", "conll",
    "-outputDirectory", targetDir,
    "-filelist", filelistFile
  )
  cmd <- paste(cmdRaw, collapse = " ")
  cmd
})
