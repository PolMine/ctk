#' @include ctkPipe_class.R
NULL


setGeneric("rsync", function(object, ...) standardGeneric("rsync"))

#' @exportMethod rsync
#' @rdname ctkPipe
setMethod("rsync", "ctkPipe", function(object){
  cmd <- c(
    "rsync", "-avzbe", "ssh",
    object@projectDir,
    object@remoteDir
  )
  cmd <- paste(cmd, collapse=" ")
  print(cmd)
})

