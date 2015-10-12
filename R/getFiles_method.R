#' @exportMethod getFiles
setGeneric("getFiles", function(.Object, ...) standardGeneric("getFiles"))

#' get files from several directories
#'  
#' @param .Object a ctkPipe class object
#' @param sourceDir source directories
#' @param targetDir targetDir in the projectDir
#' @param recursive whether to check subdirectories
#' @param exclude I do not remember
#' @param rectify logical, whether to get rid of special characters in filenames
#' @param method either list.files or find
#' @param pattern the pattern for the filenames
#' @param verbose logical, whether to be verbose
#' @param progress whether to show progress bar
#' @param failsafe whether to be robust
#' @rdname getFiles-method
#' @name getFiles
setMethod(
  "getFiles", "ctkPipe",
  function(
    .Object, sourceDir, targetDir,
    recursive=TRUE, exclude=NULL, rectify=TRUE, method="find",
    pattern="html", verbose=T, progress=F, failsafe=FALSE
    ){
  Sys.setlocale(category="LC_ALL", locale="de_DE.UTF-8")
  filesToGet <- unlist(
    lapply(sourceDir, function(x) {
      if (method == "list.files"){
        toReturn <- list.files(x, recursive=recursive, pattern=pattern, full.names=TRUE)
      } else if (method == "find"){
        cmd <- paste(
          "find", x, "-name", paste('"*.', pattern, '"', sep=""), collapse=" "
          )
        toReturn <- system(cmd, intern=TRUE)
      }
      toReturn
    })
  )
  filesToGetList <- strsplit(filesToGet, split="/")
  if (rectify == TRUE){
    targetFilenames <- sapply(
      filesToGetList,
      function(x){
        last <- length(x)
        x[last] <- gsub("\\s", "_", x[last], perl=T)
        toReplace <- list(
          c("ä", "ae"), c("ü", "ue"), c("ö", "oe"),
          c("Ä", "Ae"), c("Ü", "Ue"), c("Ö", "Oe"), 
          c("ß", "ss")
          )
        for (r in toReplace) x[last] <- gsub(r[1], r[2], x[last])
        x[last] <- gsub("[\\W]", "", x[last], perl=T)
        x[last] <- gsub("html$", ".html", x[last], perl=T)
        gsub("tei$", ".tei", x[last], perl=T)
      })
  } else {
    targetFilenames <- sapply(filesToGetList, function(x) x[length(x)])
  }
  if (length(filesToGet) > 0){
    foo <- sapply(
      c(1: length(filesToGet)),
      function(i) {
        fileToGet <- filesToGet[i]
        targetFilename <- targetFilenames[i]
        if (progress == T) .progressBar(i=i, total=length(filesToGet))
        if (verbose == T) message("... getting ", targetFilename)
        if (failsafe == FALSE){
          file.copy(from=fileToGet, to=file.path(.Object@projectDir, targetDir, targetFilename))        
        } else {
          try(
            file.copy(from=fileToGet, to=file.path(.Object@projectDir, targetDir, targetFilename))
            )
        }
        
      })    
  }
  return(length(foo))
})

# toReplace <- list(
#   c("\"", ""),
#   c("\\(", "\\ ")
# )
# 
# for (r in toReplace){
#   r[1], r[2]
# }
# 
# 
# "[\"\\(\\?\\&\\+]\\[\\]\\!\\$", ""
# "", ""
# "", "_"
# "\\*", ""
# "\"", ""
# "\\(", "\\ "
# "\\?", " "
# "\\&|\\+", " "
# "\\[|\\]", ""
# "\\!", ""
# "\\$", "_"
# "\\*", ""
# "\"", ""
# "\\(", "\\ "
# "\\?", " "
# "\\&|\\+", " "
# "\\[|\\]", ""
# "\\!", ""
# "\\$", "_"
# "\\*", ""
# "^ ", ""
# "\"", ""
# "^  ", ""
# "\"", ""
# "^  ", ""
# "\\(", "\\ "
# "\\?", " "
# "\\&|\\+", " "
# "\\[|\\]", ""
# '\\"', ''
# "\\«|\\»", ""
# "^ ", ""
# "^#", ""
# "\"", ""
# "^  ", ""
# "\\(|\\)", "\\ "
# "\\?", " "
# "\\&|\\+", " "
# "\\[|\\]", ""
# "\"", ""
# "^  ", ""
# "\\(", "\\ "
# "\\)", "\\ "
# "\\?", " "
# "\\&|\\+", " "
# "\\[|\\]", ""
# "\\//|\\/", " "
# "\\-", "_"
# "\\!", "."