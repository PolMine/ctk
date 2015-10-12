#' get files in targetDir that are missing in sourceDir
#' 
#' @param .Object ctkObject
#' @param sourceDir bla
#' @param targetDir bla
#' @param ...
#' @rdname missingFiles
#' @name missingFiles
#' @exportMethod missingFiles
setGeneric("missingFiles", function(.Object, ...) standardGeneric("missingFiles"))

#' @rdname missingFiles
setMethod("missingFiles", "ctkPipe", function(.Object, sourceDir, targetDir){
  filesTargetDir <- list.files(file.path(.Object@projectDir, targetDir))
  filesSourceDir <- list.files(file.path(.Object@projectDir, sourceDir))
  filesTargetDirChomp <- gsub("^(.*)\\..*$", "\\1", filesTargetDir)
  filesSourceDirChomp <- gsub("^(.*)\\..*$", "\\1", filesSourceDir)
  missingFiles <- filesSourceDir[!filesSourceDirChomp %in% filesTargetDirChomp]
  list(sourceDir=filesSourceDir, targetDir=filesTargetDir, missing=missingFiles)
})
