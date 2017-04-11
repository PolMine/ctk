#' @include pipe_class.R
NULL


setGeneric("rsync", function(object, ...) standardGeneric("rsync"))

#' @exportMethod rsync
#' @rdname pipe
setMethod("rsync", "pipe", function(object){
  cmd <- c(
    "rsync", "-avzbe", "ssh",
    object@projectDir,
    object@remoteDir
  )
  cmd <- paste(cmd, collapse=" ")
  print(cmd)
})

