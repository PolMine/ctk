setGeneric("checkDirs", function(.Object, ...) standardGeneric("checkDirs"))

#' check whether necessary directories exist
#' 
#' @param .Object
#' @param sourceDir directory1
#' @param targetDir
#' @exportMethod checkDirs
#' @rdname ctkPipe-class
setMethod("checkDirs", "ctkPipe", function(.Object, sourceDir, targetDir){
  if (!sourceDir %in% list.dirs(.Object@projectDir, full.names=FALSE)) {
    warning("sourceDir not found")
  }
  if (!targetDir %in% list.files(.Object@projectDir, full.names=FALSE)) {
    dir.create(file.path(.Object@projectDir, targetDir))
  }
  return(NULL)
})