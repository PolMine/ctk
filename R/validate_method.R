.validate <- function(filename, sourceDir, targetDir = NULL, param = list(), verbose = FALSE){
  cmd <- "xmllint"
  if (!is.null(param[["dtd"]])) cmd <- c(cmd, "--loaddtd", param[["dtd"]])
  if (is.null(targetDir)) cmd <- c(cmd, "--noout")
  cmd <- c(cmd, file.path(sourceDir, filename))
  if (!is.null(targetDir)) cmd <- c(cmd, "--recover", ">", file.path(targetDir, filename))
  cmd <- paste(cmd, collapse = " ")
  if (verbose) cat(cmd)
  msg <- system(cmd, ignore.stderr = FALSE, intern = FALSE)
  if (verbose) cat(msg)
  if (msg == 0) TRUE else FALSE
}


#' @name validate
#' @title Validate XML files.
#' @description A single file, or all files in directory x are checked for
#'   validity using the command line tool \code{xmllint}. If \code{targetDir} is
#'   specified, the option '--recover' will be used, and repaired XML files are
#'   written to targetDir. If the param \code{dtd} provides a filename with a
#'   DTD, files are checked against the respective DTD.
#' @param x character vector, a filename, or a directory path
#' @param targetDir the target directory
#' @param dtd filename of a DTD
#' @param ... arguments that will be passed into dirApply
#' @export validate
#' @rdname validate
#' @examples
#' \dontrun{
#'   validate("/Users/blaette/Lab/repos/plprnwhtm/html/2015-02-23/utf8", dtd=NULL)
#'   for i in $(ls); do xmllint --recover $i -o ../xmlValidated/$i; done
#' }
#' @rdname validate
#' @aliases validate
validate <- function(x, targetDir = NULL, dtd = NULL, ...){
  if (file.info(x)[["isdir"]] == FALSE){
    y <- .validate(
      filename = basename(x), sourceDir = dirname(x), targetDir = targetDir,
      param = list(dtd = dtd)
      )
  } else {
    y <- dirApply(f = .validate, sourceDir = x, targetDir = targetDir, param = list(dtd = dtd), ...)
  }
  y
}