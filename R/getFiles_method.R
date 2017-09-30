#' Get files from several directories.
#'  
#' Files in the sourceDir are copied to the targetDir. The function
#' offers options for a robust process.
#'  
#' @param sourceDir source directories
#' @param targetDir targetDir target directory
#' @param recursive whether to check subdirectories
#' @param exclude I do not remember
#' @param rectify logical, whether to get rid of special characters in filenames
#' @param method either list.files or find
#' @param pattern a pattern filenames should match, passed into \code{list.files}
#' @param verbose logical, whether to be verbose
#' @param progress whether to show progress bar
#' @param failsafe whether to be robust
#' @rdname getFiles
#' @name getFiles
#' @import pbapply
#' @export .getFiles
.getFiles <- function(
    sourceDir, targetDir,
    recursive = TRUE, exclude = NULL, rectify = TRUE, method = "find",
    pattern = NULL, verbose = TRUE, progress = TRUE, failsafe = FALSE
    ){
  Sys.setlocale(category = "LC_ALL", locale = "de_DE.UTF-8")
  filesToGet <- unlist(
    lapply(sourceDir, function(x) {
      if (method == "list.files"){
        toReturn <- list.files(x, recursive = recursive, pattern = pattern, full.names = TRUE)
      } else if (method == "find"){
        cmd <- paste(
          "find", x, "-name", paste('"*.', pattern, '"', sep=""), collapse=" "
          )
        toReturn <- system(cmd, intern = TRUE)
      }
      toReturn
    })
  )
  filesToGetList <- strsplit(filesToGet, split = "/")
  message("Files to get: ", length(filesToGetList))
  if (rectify == TRUE){
    targetFilenames <- sapply(
      filesToGetList,
      function(x){
        last <- length(x)
        x[last] <- gsub("\\s", "_", x[last], perl=T)
        toReplace <- list(
          c("\uE4", "ae"), c("\uFC", "ue"), c("\uF6", "oe"),
          c("\uC4", "Ae"), c("\uDC", "Ue"), c("\uD6", "Oe"), 
          c("\uDF", "ss")
          )
        for (r in toReplace) x[last] <- gsub(r[1], r[2], x[last])
        x[last] <- gsub("[\\W]", "", x[last], perl=T)
        x[last] <- gsub("html$", ".html", x[last], perl=T)
        x[last] <- gsub("(TXT|txt)$", ".txt", x[last], perl=T)
        gsub("tei$", ".tei", x[last], perl=T)
        gsub("xml$", ".xml", x[last], perl = TRUE)
      })
  } else {
    targetFilenames <- sapply(filesToGet, basename)
  }
  if (length(filesToGet) > 0){
    fileGetter <- function(i) {
      fileToGet <- filesToGet[i]
      targetFilename <- targetFilenames[i]
      if (verbose) message("... getting ", targetFilename)
      targetFile <- file.path(targetDir, targetFilename)
      if (failsafe == FALSE){
        file.copy(from = fileToGet, to = targetFile)
      } else {
        try(
          file.copy(from = fileToGet, to = targetFile)
        )
      }
      return(fileToGet)
    }
    if (progress){
      verbose <- FALSE
      filesGot <- pbapply::pblapply(1:length(filesToGet), fileGetter)
    } else {
      filesGot <- lapply(1:length(filesToGet), fileGetter)
    }
      
    message("Number of files copied: ", length(filesGot))
    invisible(targetFilenames)
  } else {
    message("no files to get")
    return( character() )
  }
}
