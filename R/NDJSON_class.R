#' Parse Stanford CoreNLP JSON output.
#' 
#' The JSON results of applying the Stanford CoreNLP annotators can be written
#' to a streaming JSON file (ndjson format). The NDJSON class offers functionality
#' to parse this kind of data, and to make it available in a tabular format.
#' 
#' @field destfile a character string naming the file to write to
#' @field logfile character string naming file for error log
#' @field colsToKeep character vector with columns of the parsed Stanford
#'   CoreNLP result to keep
#'   
#' @param x character vector, the JSON string(s) to be parsed
#' @param colsToKeep columns to keep
#' @param destfile a character string naming the file to write to
#' @param logfile a character string naming the file to an error log to; if
#'   provided, json strings will be written to this file if parsing the json
#'   string string fails
#'   
#' @export NDJSON
#' @rdname NDJSON
#' @importFrom data.table fread rbindlist as.data.table
#' @importFrom jsonlite fromJSON
NDJSON <- setRefClass(
  
  "NDJSON",
  
  fields = list(
    destfile = "character",
    logfile = "character",
    colsToKeep = "character"
  ),
  
  methods = list(
    
    initialize = function(destfile = character(), logfile = character(), colsToKeep = c("sentence", "index", "word", "pos", "ner")){
      
      "Initialize a new instance of the CoreNLP/NDJSON parser."
      
      .self$destfile <- destfile
      .self$logfile <- logfile
      .self$colsToKeep <- colsToKeep
    },
    
    jsonToDf = function(x){
      
      "Parse a json string to a data.frame. If a destfile has been defined during initialization,
      the output will be appended to the file provided. Without a destfile, a data.frame
      is returned. Strings that cannot be parsed are written to the logfile, if it is defined."
      
      stopifnot(is.character(x))
      
      if (length(x) == 1){
        # run the parsing within try - coding issues may cause problems
        dat <- try( jsonlite::fromJSON(x) )
        if (is(dat)[1] == "try-error"){
          warning("Cannot parse character vector: ", x)
          if (length(.self$logfile) > 0) cat(x, file = .self$logfile, append = TRUE)
          return(NULL)
        }
        
        # to cope with '{"chunk": 2859285,  "sentences": [ ] }'
        if (length(dat$sentences$tokens) == 0){
          warning("JSON string without tokens: ", x)
          if (length(.self$logfile) > 0) cat(x, file = logfile, append = TRUE)
          return(NULL)
        }
        
        dfs <- lapply(
          1:length(dat$sentences$tokens),
          function(i){
            if (ncol(dat$sentences$tokens[[i]]) > 0){
              return( data.frame(sentence = i, dat$sentences$tokens[[i]]))
            } else {
              return( NULL )
            }
          }
        )
        df <- do.call(rbind, dfs)
        
        # add chunk number, if present, and 
        if ("chunk" %in% names(dat)){
          df[["chunk"]] <- dat[["chunk"]]
          cols <- c("chunk", .self$colsToKeep)
        } else {
          cols <- .self$colsToKeep
        }
        y <- df[, cols]
        
        # output
        if (length(.self$destfile) > 0){
          write.table(
            y, file = .self$destfile,
            sep = "\t", append = TRUE, row.names = FALSE,
            col.names = if (file.exists(destfile)) FALSE else TRUE
          )
        } else {
          return(y)
        }
      } else if (length(x) > 1){
        dfs <- pbapply::pblapply(x, function(x) .self$jsonToDf(x))
        return( do.call(rbind, dfs) )
      }
    },
    
    processFiles = function(filenames){
      
      "Process one or more files with the output of Stanford CoreNLP in a NDJSON format. If
      a destfile has been defined during initialization, results are written/appended to that
      file. Otherwise, a data.frame is returned."
      
      if (length(.self$destfile) == 1){
        started <- Sys.time()
        for (filename in filenames){
          con <- file(filename, "r")
          while ( TRUE ) {
            line <- readLines(con, n = 1)
            if ( length(line) == 0 ) break
            .self$jsonToDf(line)
          }
          close(con)
        }
        return(Sys.time() - started)
      } else {
        dfs <- pbapply::pblapply(
          filenames,
          function(filename){
            con <- file(filename, "r")
            toParse <- readLines(con, n = 1)
            df <- .self$jsonToDf(toParse)
            close(con)
            df
          }
        )
        return( do.call(rbind, dfs) )
      }
    }
  )
)
