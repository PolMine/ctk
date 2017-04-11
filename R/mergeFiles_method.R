setGeneric("mergeFiles", function(.Object, ...) standardGeneric("mergeFiles"))


#' merge files
#' 
#' Merge files matching a regex to a new XML file.
#' 
#' @param .Object a ctkObject
#' @param sourceDir the source directory
#' @param targetDir the target directory
#' @param regex the regex to match the files
#' @param mc logical, whether to use parallel processing
#' @param verbose logical, whether to be verbose
#' @param ... further parameters
setMethod("mergeFiles", "pipe", function(
  .Object, sourceDir, targetDir, regex, rootElement, rootAttrs,
  mc=FALSE, verbose=TRUE, ...
  ){
  if (verbose == TRUE) message("... getting files in sourceDir")
  files <- list.files(file.path(.Object@projectDir, sourceDir))
  matches <- unique(gsub(regex, "\\1", files))
  fileEnding <- unique(gsub("^.*(\\..*)$", "\\1", files))
  if (length(fileEnding) > 1) warning("... WARNING: various file endings present in this dir")
  newFiles <- paste(matches, fileEnding[1], sep="")
  newRegexSet <- sapply(matches, function(x) gsub("\\(.*\\)", x, regex))
  if (verbose == TRUE) message("... grouping the files")
  filesByGroups <- lapply(newRegexSet, function(newRegex) grep(newRegex, files, value=TRUE) )
  lengthsFileSet <- lapply(
    c(1:length(filesByGroups)),
    function(noFileSet){
      fileSet <- filesByGroups[[noFileSet]]
      nameFileSet <- names(filesByGroups)[noFileSet]
      if (verbose == TRUE) message("... processing group ", nameFileSet)
      fileName <- newFiles[[noFileSet]]
      newFile <- file.path(.Object@projectDir, targetDir, fileName)
      lengthFileSet <- lapply(
        c(1:length(fileSet)),
        function(i){
          input <- scan(
            file=file.path(.Object@projectDir, sourceDir, fileSet[i]),
            what="character", sep="\n", quiet=TRUE
          )
          posXmlDeclaration <- grep("^\\s*<\\?xml.*", input)
          if (i == 1) {
           rootAttrsChar <- paste(
             "",
             sapply(
               c(1:length(rootAttrs)),
               function(x){ paste(names(rootAttrs[x]), '="', rootAttrs[x],'"', sep='') }
             ),
             collapse='')
           newRootNode <- paste('<', rootElement, ' group="', nameFileSet, '"', rootAttrsChar, '>',  sep='')
           output <- c(input[posXmlDeclaration], newRootNode, input[c((posXmlDeclaration+1):length(input))])
           cat(output, file=newFile, sep="\n")
         } else {
           cat(input[-posXmlDeclaration], file=newFile, sep="\n", append=TRUE)
         }
        return(setNames(length(fileSet), nameFileSet))
      })
      cat(paste('</', rootElement, '>', sep=""), file=newFile, sep="\n", append=TRUE)
      return(lengthFileSet)
    })
  return(lengthsFileSet)
})