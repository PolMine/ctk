#' @name xslt
#' @title Perform XSL transformation.
#' @description Worker function to be iterated over all files using \code{dirApply}.
#' @param filename XML file to be processed
#' @param sourceDir directory where the XML file resides
#' @param targetDir output directory
#' @param verbose logical value, whether to be verbose
#' @param param named list of further parameters, needs to include \code{xslFile},
#'   the filename of the XSL file
#' @rdname xslt
#' @export xslt
#' @return return of the saxon parser
xslt <- function(filename, sourceDir = NULL, targetDir = NULL, verbose = FALSE, param = list(xslFile = character())){
  if (length(param[["xslFile"]]) == 0) stop("xslFile needs to be defined")
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