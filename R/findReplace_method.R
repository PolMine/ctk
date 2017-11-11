#' Find and replace.
#' 
#' @param x character vector either specifying a file directory, or a string to process
#' @param y a target directory 
#' @param replacements a list
#' @param ... further parameters that are passed to \code{dirApply}
#' @export findAndReplace
#' @examples 
#' x <- c("water and milk", "butter or water", "scotch or soda")
#' findAndReplace(x, replacements = list(c("and", "or"), c("s", "S"), c("\\sw", " W")))
findAndReplace <- function(x, y = NULL, replacements, ...){
  if (file.exists(x)[1] == FALSE){
    y <- .findAndReplace(
      filename = x, sourceDir = NULL, targetDir = NULL,
      param = list(replacements = replacements)
    )
  } else { 
    if (file.info(x)[["isdir"]] == FALSE){
      y <- .findAndReplace(
        filename = basename(x), sourceDir = dirname(x), targetDir = y,
        param = list(replacements = replacements)
      )
    } else {
      y <- dirApply(f = .validate, sourceDir = x, targetDir = y, param = list(replacements = replacements), ...)
    }
  }
  y
}

.findAndReplace <- function(filename, sourceDir = NULL, targetDir = NULL, verbose = TRUE, param = list(replacements = list())){
  if (!is.null(targetDir)) startTime <- Sys.time() # if targetDir is provided, return processing time
  
  doc <- if (!is.null(sourceDir)) readLines(file.path(sourceDir, filename)) else filename
  for (replacement in param[["replacements"]]) doc <- gsub(replacement[1], replacement[2], doc)
  
  if (is.null(targetDir)){
    return(doc)
  } else {
    cat(doc, sep = "\n", file = file.path(targetDir, filename))
    return( Sys.time() - startTime )
  }
}
