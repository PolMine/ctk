setGeneric("replaceInvalidCharacters", function(.Object, ...) standardGeneric("replaceInvalidCharacters"))

.replace <- function (txt){
  charactersToReplace <- list(
    #  c("?", " "),
    c("[\001]", " "), c("[\002]", " "), c("[\003]", " "), c("[\004]", " "), c("[\005]", " "), c("[\006]", " "),
    c("[\016]", " "), c("[\017]", " "), c("[\020]", " "), c("[\021]", " "), c("[\022]", " "), c("[\023]", " "), c("[\024]", " "), c("[\025]", " "), c("[\026]", " "), c("[\027]", " "),
    c("[\031]", " "), c("[\032]", " "), c("[\033]", " "), c("[\034]", " "), c("[\035]", " "), c("[\036]", " "), c("[\037]", " "),
    c("[\177]", " "),
    c("\u0081", " "), c("\u0084", " "), c("\u0094", " "), c("\u009a", " "), c("\u008e", " "), c("\u0099", " "),
    c("\u0082", " "), c("\u0088", " "), c("\u008a", " "), c("\u0093", " "), c("\u0080", " "), c("\u0096", " "),
    c("\u0097", " "), c("\u009f", " "), c("\u0086", " "), c("\u0083", " "), c("\u0087", " "), c("\u0089", " "),
    c("\u008b", " "), c("\u008f", " "), c("\u0092", " "), c("\u009b", " "), c("\u009c", " "), c("\u009d", " "),
    c("\u009e", " "), c("\u0098", " "), c("\u008c", " "), c("\u0095", " "), c("\u0091", " "), c("\u0085", " "),
    c("\a", " "), c("\b", " "),  c("\f", " "), c("\t", " "), c("\v", " ")
  )
  for (i in c(1:length(charactersToReplace))) txt <- gsub(charactersToReplace[[i]][1], charactersToReplace[[i]][2], txt)
  return(txt)
}


.replaceInTxtFile <- function(filename, sourceDir, targetDir, verbose, param=list()){
  startTime <- Sys.time()
  if (is.null(targetDir)) targetDir <- sourceDir
  txt <- scan(
    file.path(sourceDir, filename),
    what="character", sep="\n", blank.lines.skip=FALSE, quiet=TRUE
  )
  txt <- .replace(txt)
  cat(txt, file=file.path(targetDir, filename), sep="\n")
  return(Sys.time() - startTime)
}


#' replace invalid characters
#' 
#' Replace characters that do not conform to UTF-8 encoding. If txt is NULL, all files
#' in the source dir are processed.
#' 
#' @param txt defaults to NULL, if provided, a text string to be processed
#' @param sourceDir directory with files to be processed
#' @param targetDir defaults to NULL, if provided, the directory where the processed files are saved
#' @param xml defaults to FALSE, if TRUE, files are provessed as XML files (not yet implemented)
#' @param filenames defaults to NULL, if provided, a set of files in the sourceDir
#' @param mc logcial or numeric, whether to use multicore and the number of cores
#' @param progress logical
#' @param verbose logical 
#' @export replaceInvalidCharacters
#' @rdname replaceInvalidCharacters
#' @name replaceInvalidCharacters
setMethod("replaceInvalidCharacters", "ctkPipe", function(.Object, sourceDir, targetDir, xml=FALSE, ...){
  if (xml == FALSE) {
    stopifnot(!is.null(sourceDir))
    dirApply(
      f=.replaceInTxtFile,
      sourceDir=file.path(.Object@projectDir, sourceDir),
      targetDir=file.path(.Object@projectDir, targetDir),
      param=list(), ...
      )
  } else if (xml == TRUE){
    stop("replacing invalid characters in XML files not yet implemented")
  }
})