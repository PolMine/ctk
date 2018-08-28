#' @name install.treetagger
#' @title Install treetagger.
#' @description The Treetagger is a standard tool for part-of-speech annotation
#' and lemmatization
#' @param lang Languages to install.
#' @export install.treetagger
#' @rdname install.treetagger   
install.treetagger <- function(lang = "de"){
  # create necessary directories
  ctkDir <- system.file(package = "ctk")
  exttoolsDir <- file.path(ctkDir, "exttools")
  if (!file.exists(exttoolsDir)) dir.create(exttoolsDir)
  treetaggerDir <- file.path(exttoolsDir, "treetagger")
  if (!file.exists(treetaggerDir)) dir.create(treetaggerDir)
  
  # download treetagger
  if (.Platform$OS.type != "unix") stop("Aborting - treetagger is only available for unix")
  if (Sys.info()["sysname"] == "Darwin"){
    ttURL <- "http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/tree-tagger-MacOSX-3.2.tar.gz"
  } else {
    ttURL <- "http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/tree-tagger-linux-3.2.1.tar.gz"
  }
  treetaggerZipFile <- basename(ttURL)
  taggingScriptsURL <- "http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/tagger-scripts.tar.gz"
  download.file(ttURL, destfile = file.path(treetaggerDir, treetaggerZipFile))
  download.file(taggingScriptsURL, destfile = file.path(treetaggerDir, basename(taggingScriptsURL)))
  installerScript <- "http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/install-tagger.sh"
  download.file(installerScript, destfile = file.path(treetaggerDir, basename(installerScript)))
  parameterFiles <- c(
    "de" = "http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/german-par-linux-3.2-utf8.bin.gz"
  )
  for (x in lang){
    download.file(parameterFiles[x], destfile = file.path(treetaggerDir, basename(parameterFiles[x])))
  }
  system(paste("sh", file.path(treetaggerDir, "install-tagger.sh"), sep = " "))
}
