#' @include ctkPipe_class.R
#' @import methods
NULL



#' @exportMethod show
#' @rdname ctkPipe
setMethod("show", "ctkPipe", function(object){
  cat(object@projectDir)
  cat("\n\n")
  print(object@tasks)
})

#' @exportMethod summary
#' @rdname ctkPipe
setMethod("summary", "ctkPipe", function(object){
  subdirs <- list.dirs(object@projectDir, full.names=FALSE)
  subdirs <- subdirs[which(subdirs != c(""))]
  noFiles <- sapply(
    setNames(subdirs, subdirs),
    function(dir) length(list.files(file.path(object@projectDir, dir)))
  )
  retval <- data.frame(
    row.names=names(noFiles),
    noFiles=noFiles,
    describtion=object@subdirs[names(noFiles)]
    )
  return(retval)
})

