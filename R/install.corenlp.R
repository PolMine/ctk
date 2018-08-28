#' @name install.corenlp
#' @title Install Stanford CoreNLP.
#' @description The function provides an installation mechanism to download and install
#' Stanford CoreNLP within the ctk package.
#' and lemmatization
#' @param lang Languages to install.
#' @export install.corenlp
#' @rdname install.corenlp
#' @importFrom utils download.file unzip zip
install.corenlp <- function(lang = "de"){
  # create necessary directories
  ctkDir <- system.file(package = "ctk")
  exttoolsDir <- file.path(ctkDir, "exttools")
  if (!file.exists(exttoolsDir)) dir.create(exttoolsDir)
  corenlpDir <- file.path(exttoolsDir, "corenlp")
  if (!file.exists(corenlpDir)) dir.create(corenlpDir)
  
  corenlpURL <- "http://nlp.stanford.edu/software/stanford-corenlp-full-2017-06-09.zip"
  zipfile <- file.path(corenlpDir, basename(corenlpURL))
  download.file(url = corenlpURL, destfile = zipfile)
  unzip(zipfile = zipfile, exdir = corenlpDir)
  file.remove(zipfile)
  
  languages <- list(
    de = function(){
      germanJarURL <- "http://nlp.stanford.edu/software/stanford-german-corenlp-2017-06-09-models.jar"
      germanJar <- file.path(corenlpDir, "stanford-corenlp-full-2017-06-09", basename(germanJar))
      download.file(url = germanJarURL, destfile = germanJar)
      unzip(germanJar, files = "StanfordCoreNLP-german.properties")
      zip(zipfile = germanJar, files = "StanfordCoreNLP-german.properties", flags = "-d")
    }
  )
  for (language in lang) languages[[language]]
}