#' Use TreeTagger for linguistic annotation.
#' 
#' The argument \code{param} is a list passing the arguments \code{lang}, a 
#' character vector that is expected to be "de", "fr", "it", or "en". The argument
#' \code{tokenize} is a logical value. If \code{TRUE}, the tokenizer included in
#' the treetagger scripts used, if \code{FALSE}, the input is expected to be
#' tokenized already.
#' 
#' Depending whether targetDir is defined (i.e. not \code{NULL}), output is written to the
#' file, or a character vector is returned. If sourceDir is \code{NULL}, filename will serve
#' as the input character string. It will be written to a temporary file for further
#' processing.
#' 
#' @param filename file to process, or a character vector (if sourceDir is NULL)
#' @param targetDir output directory, if NULL, the processed input will be returned
#' @param param a list that needs to include the language to be used (defaults to 'de') and a logical
#' vector \code{tokenize} whether the input needs to be tokenized before tagging
#' @param verbose logical, defaults to TRUE
#' @rdname treetagger
#' @export treetagger
#' @name treetagger
#' @importFrom utils capture.output
#' @importFrom tools file_path_sans_ext
treetagger <- function(filename, sourceDir = NULL, targetDir = NULL, verbose = FALSE, param = list(lang = "de", tokenize = TRUE)){
  
  stopifnot(param[["lang"]] %in% c("de", "fr", "it", "en"))
  stopifnot("tokenize" %in% names(param))
  stopifnot(getOption("ctk.treetaggerDir") != "")
  
  startTime <- Sys.time()
  
  if (param[["tokenize"]] == TRUE){
    cmdFile <- switch(
      param[["lang"]],
      de = file.path(getOption("ctk.treetaggerDir"), "cmd", "tree-tagger-german"),
      fr = file.path(getOption("ctk.treetaggerDir"), "cmd", "tree.tagger-french"),
      it = file.path(getOption("ctk.treetaggerDir"), "cmd", "tree-tagger-italian"),
      en = file.path(getOption("ctk.treetaggerDir"), "cmd", "tree-tagger-english")
    )
    if (is.null(sourceDir)){
      if (!is.null(targetDir)) warning("If sourceDir is NULL, treetagger output is not written to a file!")
      tmpfile <- tempfile()
      cat(filename, file = tmpfile)
      cmdRaw <- c("cat", tmpfile, "|", cmdFile)
      cmd <- paste(cmdRaw, collapse = " ")
      retval <- system(cmd, intern = TRUE, ignore.stdout = FALSE, ignore.stderr = TRUE)
      return(retval)
    } else {
      cmdRaw <- c(cmdFile, file.path(sourceDir, filename))
      if (is.null(targetDir)){
        cmd <- paste(cmdRaw, collapse = " ")
        retval <- system(cmd, intern = TRUE, ignore.stdout = FALSE, ignore.stderr = TRUE)
        return(retval)
      } else {
        cmdRaw <- c(
          cmdRaw, ">",
          file.path(targetDir, paste(tools::file_path_sans_ext(filename), "vrt", sep = "."))
        )
        cmd <- paste(cmdRaw, collapse = " ")
        system(cmd, intern = TRUE, ignore.stdout = FALSE, ignore.stderr = TRUE)
        return( Sys.time() - startTime)
      }
    }
  } else {
    if (is.null(sourceDir)){
      stop("Using treetagger without tokenization requires that the input is read from a file.")
    }
    parFile <- switch(
      param[["lang"]],
      de = file.path(getOption("ctk.treetaggerDir"), "lib", "german-utf8.par"),
      fr = file.path(getOption("ctk.treetaggerDir"), "lib", "french-utf8.par"),
      it = file.path(getOption("ctk.treetaggerDir"), "lib", "italian-utf8.par"),
      en = file.path(getOption("ctk.treetaggerDir"), "lib", "english-utf8.par")
    )
    cmdRaw <- c(
      file.path(getOption("ctk.treetaggerDir"), "bin", "tree-tagger"),
      "-sgml", "-token", "-lemma",
      parFile,
      file.path(sourceDir, filename)
      )
    if (is.null(targetDir)){
      cmd <- paste(cmdRaw, collapse = " ")
      retval <- system(cmd, intern = TRUE, ignore.stdout = FALSE, ignore.stderr = TRUE)
      return(retval)
    } else {
      cmdRaw <- c(cmdRaw, file.path(targetDir, filename))
      cmd <- paste(cmdRaw, collapse = " ")
      system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
      return( Sys.time() - startTime)
    }
  }
}
