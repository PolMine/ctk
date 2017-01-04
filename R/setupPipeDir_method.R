#' Setup pipeDir.
#' 
#' Create pipeDir and all relevant subdirectories.
#' @param .Object the object
#' @param sub the subdirectories to create (character vector)
#' @param main the main directory
#' @rdname setupPipeDir
#' @exportMethod setupPipeDir
setGeneric("setupPipeDir", function(.Object, ...) standardGeneric("setupPipeDir"))

setMethod("setupPipeDir", "character", function(.Object, sub = NULL){
  
  if (!dir.exists(.Object)) {
    dir.create(.Object)
    message("Creating (main pipeDir) directory: ", .Object)
  } else {
    message("Main directory already exists, it is not created anew.")
  }
  
  for (subdir in sub){
    newSubdir <- file.path(.Object, subdir)
    if (!dir.exists(newSubdir)){
      dir.create(newSubdir)
      message("Creating directory: ", .Object)
    } else {
      message("Subdirectory ", subdir, " already exists, is not created anew.")
    }
  }
})