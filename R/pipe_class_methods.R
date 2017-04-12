#' @include pipe_class.R
#' @import methods
NULL



#' @exportMethod show
#' @rdname pipe
setMethod("show", "pipe", function(object){
  cat(object@projectDir)
  cat("\n\n")
  print(object@tasks)
})

#' @exportMethod summary
#' @rdname pipe
setMethod("summary", "pipe", function(object){
  subdirs <- list.dirs(object@projectDir, full.names=FALSE)
  subdirs <- subdirs[which(subdirs != c(""))]
  noFiles <- sapply(
    setNames(subdirs, subdirs),
    function(dir) length(list.files(file.path(object@projectDir, dir)))
  )
  retval <- data.frame(
    row.names = names(noFiles),
    no_files = noFiles,
    description = object@subdirs[names(noFiles)]
    )
  return(retval)
})

