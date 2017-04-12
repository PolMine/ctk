setGeneric("getNestedElements", function(.Object, ...) standardGeneric("getNestedElements"))


setMethod("getNestedElements", "pipe", function(.Object, sourceDir, corpus, element, max.embedding = NULL){
  if (is.null(max.embedding)){
    els <- grep(paste(element, "\\d+", sep = ""), sAttributes(corpus), value = TRUE)
    max.embedding <- max(as.integer(gsub("^.*?(\\d+)$", "\\1", els)))
  }
  cposList <- lapply(
    1:max.embedding,
    function(x) CQI$struc2cpos(corpus, paste(element, x, sep = ""), 0)
  )
  cposDf <- data.frame(do.call(rbind, cposList))
  text <- lapply(
    1:nrow(cposDf),
    function(i){
      start <- cposDf[i,1]
      end <- cposDf[i,2]
      if ((end - start) > 5) end <- start + 5
      CQI$id2str(corpus, "word", CQI$cpos2id(corpus, "word", start:end))
    }
  )
  files <- lapply(
    text,
    function(x){
      cmdChunks <- c(
        "find",
        file.path(.Object@projectDir, sourceDir),
        "-type f | xargs pcregrep -M --files-with-matches",
        paste('"', paste("\\n", paste(x, collapse = ".*?\\n"), sep = ""), '"', sep = "")
      )
      cmd <- paste(cmdChunks, sep = " ", collapse = " ")
      cat(cmd)
      cat("\n")
      result <- system(cmd, intern = TRUE)
      paste(result, collapse = "|")
    }
  )
  cposDf[["text"]] <- lapply(text, function(x) paste(x, collapse = " "))
  cposDf[["file"]] <- unlist(files)
  cposDf
})