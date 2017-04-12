#' @exportMethod subset
setMethod("subset", "pipe", function(x, sourceDir, targetDir, sample=NULL, files=NULL){
  if (is.null(files)){
    files <- list.files(file.path(x@projectDir, sourceDir))
  }
  if (!is.null(sample)){
    files <- sample(files, size=sample)    
  }
  lapply(
    files,
    function(file){
      file.copy(
        from=file.path(x@projectDir, sourceDir, file),
        to=file.path(x@projectDir, targetDir, file)
      )  
    })
})