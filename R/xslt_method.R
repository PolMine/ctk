#' perform XSL transformation
#' 
#' @param .Object a directory
#' @param targetDir output directory
#' @param xslFile filename
#' @param mc logical, whether to use multicore
#' @param verbose whether to be verbose, defaults to TRUE
#' @rdname xslt-method
#' @exportMethod xslt
#' @param xslFile file for the xsl transformation
#' @param mkdir logical, whether to create the outDir, if it does not yet exist
#' @return return of the saxon parser
#' @exportMethod xslt
setGeneric("xslt", function(.Object, ...) standardGeneric("xslt"))

.xsltWorker <- function(filename, sourceDir = NULL, targetDir = NULL, verbose = FALSE, param = list()){
  if (is.null(targetDir)){
    targetDir <- tempdir()
    returnString <- TRUE
  } else {
    returnString <- FALSE
    startTime <- Sys.time()
  }
  if (is.null(sourceDir)){
    if (returnString == TRUE){
      xmlDir <- file.path(targetDir, "xml")
      if (file.exists(xmlDir) == FALSE) dir.create(file.path(targetDir, "xml"))
      sourceDir <- xmlDir
    } else {
      sourceDir <- tempdir()
    }
    tmpFilenamePattern <- ifelse("thread" %in% names(param), param[["thread"]], "thread1")
    tmpFilename <- tempfile(pattern=tmpFilenamePattern, tmpdir=sourceDir, fileext=".xml")
    tmpFilename <- strsplit(tmpFilename, "/")[[1]][length(strsplit(tmpFilename, "/")[[1]])]
    cat(filename, file=file.path(sourceDir, tmpFilename), sep="\n")
    filename <- tmpFilename
  }
  xslFile <- param[["xslFile"]]
  pathSaxon <- Sys.getenv("PATH_SAXON")
  cmd <- c(
    "java", "-cp", pathSaxon, "net.sf.saxon.Transform", "-t",
    paste("-s:", file.path(sourceDir, filename), sep=""),
    paste("-xsl:", xslFile, sep=""),
    paste("-o:", file.path(targetDir, filename), sep="")
  )
  cmd <- paste(cmd, collapse=" ")
  if (verbose == TRUE) message("... processing ", filename)
  system(cmd,intern=TRUE, ignore.stdout=verbose, ignore.stderr = ifelse(verbose==TRUE, FALSE, TRUE))
  if (returnString == TRUE){
    retval <- saveXML(xmlParse(file=file.path(targetDir, filename)), indent=FALSE)
    file.remove(file.path(targetDir, filename))
    return(retval)
  } else {
    return(Sys.time() - startTime)
  }
}



setMethod("xslt", "character", function(.Object, xslFile){
  .xsltWorker(filename = .Object, param = list(xslFile = xslFile))
})
