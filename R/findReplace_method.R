# @include ctkPipe_class.R
NULL

setGeneric("findReplace", function(object, ...) standardGeneric("findReplace"))

.findReplace <- function(filename, sourceDir, targetDir, verbose, ...){
  doc <- scan(
    file=file.path(sourceDir, filename),
    what="character", sep="\n",
    blank.lines.skip=TRUE,
    quiet=TRUE
  )
  for (replacement in replacements) doc <- gsub(replacement[1], replacement[2], doc)
  cat(
    doc, sep="\n",
    file=file.path(targetDir, filename)
  )
}


#' @param replacements a list
#' @param checkValidity whether to validate XML in targetDir
#' @param progress whether to use progress bar
#' @exportMethod findReplace
#' @rdname ctkPipe
setMethod("findReplace", "ctkPipe", function(
  object, sourceDir, targetDir, replacements,
  pattern=NULL, mc=FALSE, progress=TRUE, verbose=FALSE, sample=FALSE,
  files=NULL, continue=FALSE, failsafe=FALSE, ...
  ){
  checkDirs(object, sourceDir, targetDir)
  assign("replacements", replacements, envir=.GlobalEnv)
  dirApply(
    f=.findReplace,
    x=file.path(object@projectDir, sourceDir),
    y=file.path(object@projectDir, targetDir),
    pattern=pattern, mc=mc, progress=progress, verbose=verbose, sample=sample,
    files=files, continue=continue, failsafe=failsafe, replacements=replacements, ...
    )
})


# setMethod("findReplace", "ctkPipe", function(object, sourceDir, targetDir, replacements=regexPostprocessing, checkValidity=TRUE, progress=TRUE, parallel=FALSE){
#   startTime <- Sys.time()
#   checkDirs(object, sourceDir, targetDir)
#   files <- list.files(file.path(object@projectDir, sourceDir))
#   .replace <- function(i){
#     doc <- scan(
#       file=file.path(object@projectDir, sourceDir, files[i]),
#       what="character", sep="\n",
#       blank.lines.skip=TRUE,
#       quiet=TRUE
#     )
#     for (replacement in replacements) gsub(replacement[1], replacement[2], doc)
#     cat(
#       doc, sep="\n",
#       file=file.path(object@projectDir, targetDir, files[i])
#     )
#     if (parallel == FALSE && progress == TRUE) .progressBar(i, length(files))
#   }
#   if (parallel == FALSE) foo <- lapply(1:length(files), .replace)
#   if (parallel == TRUE) foo <- mclapply(1:length(files), .replace)
#   validity <- ifelse(checkValidity == TRUE, validate(object, targetDir)[[1]][1], NA)
#   object <- updateTasks(
#     object, "findReplace",
#     startTime=startTime, endTime=Sys.time(),
#     sourceDir=sourceDir, targetDir=targetDir,
#     filesProcessed=length(files),
#     validity=validity,
#     parallel=parallel
#   )
#   return(object)
# })


