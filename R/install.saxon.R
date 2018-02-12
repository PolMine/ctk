#' @name install.saxon
#' @title Install Saxon XSLT Processor to tools directory.
#' @description The Saxon parser offers functionality to perform XSL
#'   transformations efficiently. It is used by the \code{xslt} function, which
#'   calls Saxon. This function installs the Saxon parser to the subdirectory
#'   tools/saxon of the ctk package directory.
#' @export install.saxon
#' @rdname saxon   
install.saxon <- function(){
  # create necessary directories
  ctkDir <- system.file(package = "ctk")
  exttoolsDir <- file.path(ctkDir, "exttools")
  if (!file.exists(exttoolsDir)) dir.create(exttoolsDir)
  saxonDir <- file.path(exttoolsDir, "saxon")
  if (!file.exists(saxonDir)) dir.create(saxonDir)
  
  # download saxon jars
  saxonURL <- "https://sourceforge.net/projects/saxon/files/Saxon-HE/9.8/SaxonHE9-8-0-8J.zip"
  saxonZipFile <- file.path(saxonDir, "saxon.zip")
  download.file(url = saxonURL, destfile = saxonZipFile)
  unzip(zipfile = saxonZipFile, exdir = saxonDir)
  file.remove(saxonZipFile)
}
