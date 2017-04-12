#' @include pipe_class.R
NULL

.validateWorker <- function(filename, sourceDir, verbose, dtd=NULL, targetDir=NULL){
  cmd <- c("xmllint")
  if (is.null(targetDir)) cmd <- c(cmd, "--noout")
  if (!is.null(dtd)) cmd <- c(cmd, "--loaddtd", dtd)
  if (!is.null(targetDir)) cmd <- c(cmd, "--recover")
  cmd <- c(cmd, file.path(sourceDir, filename))
  if (!is.null(targetDir)) cmd <- c(cmd, ">", file.path(targetDir, filename))
  cmd <- paste(cmd, collapse=" ")
  if (verbose == TRUE) print(cmd)
  msg <- system(cmd, ignore.stderr=TRUE, intern=TRUE)
  if (verbose == TRUE) print(msg)
  return(msg)
}

#' validate XML files
#' 
#' a story to be told
#' 
#' @param .Object either a character vector indicating a directore, or a pipe object
#' @param sourceDir the source directory
#' @param dtd check agains a dtd
#' @param ... further arguments
#' @exportMethod validate
#' @name validate
#' @rdname validate
#' @examples
#' \dontrun{
#'   validate("/Users/blaette/Lab/repos/plprnwhtm/html/2015-02-23/utf8", dtd=NULL)
#'   for i in $(ls); do xmllint --recover $i -o ../xmlValidated/$i; done
#' }
setGeneric("validate", function(.Object, ...){standardGeneric("validate")})

#' @rdname validate
setMethod("validate", "character", function(.Object, dtd=NULL, targetDir=NULL, verbose=FALSE, mc=FALSE, progress=TRUE){
  .iterateFunctionFiles(
    sourceDir=.Object, f=.validateWorker, pattern="xml", mc=mc,
    verbose=verbose, progress=progress, targetDir=targetDir, dtd=dtd
  )
})


#' @rdname validate
setMethod("validate", "pipe", function(
  .Object, sourceDir, targetDir=NULL,
  dtd=NULL, verbose=FALSE, mc=FALSE, progress=TRUE, files=NULL, pattern="xml"){
  if (!is.null(targetDir)) targetDir <- file.path(.Object@projectDir, targetDir)
  dirApply(
    f=.validateWorker,
    x=file.path(.Object@projectDir, sourceDir), 
    y=targetDir,
    dtd=dtd,
    verbose=verbose, mc=mc, progress=progress, files=files, pattern=pattern
  )
})


