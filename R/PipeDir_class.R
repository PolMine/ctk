#' Pipe for corpus preparation.
#' 
#' @field dir a pipe directory, different processing stages of the corpus will be kept in
#' subdirectories of this directory
#' @field time a data.frame with information that different processing stages have consumed
#' @field threads an integer, the number of cores to use
#' @field basetable a \code{data.table} that includes metadata and a column
#'   'text' with the full text of the corpus
#' @field metadata a \code{data.table} with the metadata for the corpus; a
#'   column 'chunk' serves as an id to link the metadata table with the other
#'   tables for corpus preparation
#' @field texttable a \code{data.table} derived from the basetable, includes
#'   only the column 'chunk' with the id, and the column 'text' with the
#'   unparsed full text
#' @field tokenstream a \code{table} with a columns with the tokenized text and
#'   further annotation of the corpus
#' 
#' @param dir the pipe directory
#' @param threads an integer, the number of threads to use
#' @param metadata a named list with xpath expressions that is passed into the
#'   xmlToDT function to extract metadata from XML documents
#' @param sourceDir a subdirectory of the pipeDir where files to be processed 
#'   are located
#' @param targetDir a subdirectory of the pipeDir where processed output is
#'   stored
#' @param cols columns that will serve as input to generate p- and s-attributes
#' @param corpus name of the CWB corpus to create
#' 
#' @importFrom pbapply pblapply
#' @importFrom parallel mclapply
#' @importFrom data.table data.table rbindlist fread fwrite
#' @export Pipe
Pipe <- setRefClass(
  
  "PipeDir",
  
  fields = list(
    dir = "character",
    time = "data.frame",
    threads = "integer",
    basetable = "data.table",
    metadata = "data.table",
    texttable = "data.table",
    tokenstream = "data.table"
  ),
  
  methods = list(
    
    initialize = function(dir, threads = 1L){
      if (!dir.exists(dir)) stop(" directory does not exist")
      .self$dir <- dir
      .self$time <- data.frame(
        start = Sys.time(), end = Sys.time(), elapsed = as.difftime(0, units = "secs"),
        row.names = "all"
        )
      .self$threads <- threads
    },
    
    xmlToBasetable = function(sourceDir = "xml", targetDir = "csv", metadata){
      
      "Extract text and metadata from XML documents, and keep the resulting
      'basetable' in the respective slot of the Pipe object."
      
      started <- Sys.time()
      
      filenames <- list.files(file.path(.self$dir, sourceDir), full.names = TRUE)
      dtList <- pbapply::pblapply(
        filenames,
        function(x) ctk::xmlToDT(x, meta = metadata), cl = .self$threads
        )
      .self$basetable <- rbindlist(dtList, fill = TRUE)
      rm(dtList)
      
      .self$updateProcessingTime(started = started, call = "xmlToDT")
      invisible(.self$basetable)
    },
    
    dissectBastable = function(targetDir = "csv"){
      
      message("... making and writing texttable.csv")
      .self$texttable <- .self$basetable[, c("chunk", "text"), with = TRUE]
      data.table::fwrite(
        .self$texttable,
        file = file.path(.self$dir, targetDir, "texttable.csv")
        )
      
      message("... making and writing metadata.csv")
      .self$metadata <- .self$basetable[, text := NULL]
      data.table::fwrite(
        .self$metadata,
        file = file.path(.self$dir, targetDir, "metadata.csv"),
        quote = TRUE
        )
    },
    
    corenlp = function(sourceDir = "csv", targetDir = "ndjson"){
      
      "Use Stanford CoreNLP."
      
      started <- Sys.time()
      foo <- 1
      if (foo == 0){
        Annotator <- CoreNLP$new(
          method = "json",
          filename = file.path(.self$dir, targetDir, "annotation.ndjson"),
          stanfordDir = .self$stanfordDir, propertiesFile = .self$propertiesFile
        )
        dummy <- pbapply::pblapply(
          1:nrow(.self$texttable),
          function(i) Annotator$annotate(.self$texttable[["text"]][i], chunk = i)
        )
      } else {
        
        chunks <- text2vec::split_into(1:nrow(.self$texttable), n = .self$threads)
        outfiles <- sprintf(
          file.path(.self$dir, targetDir, "corenlp_%d.ndjson"),
          1:.self$threads
        )
        dummy <- parallel::mclapply(
          1:length(chunks),
          function(i){
            options(java.parameters = "-Xmx4g")
            Annotator <- CoreNLP$new(
              method = "json", filename = outfiles[i],
              stanfordDir = getOption("ctk.stanfordDir"),
              propertiesFile = getOption("ctk.propertiesFile")
            )
            lapply(chunks[[i]], function(j) Annotator$annotate(.self$texttable[["text"]][j], chunk = j))
            return( outfiles[i] )
          }
          , mc.cores = .self$threads
        )
      }

    },
    
    ndjsonToDf = function(sourceDir = "ndjson", targetDir = "csv"){
      
      "Convert ndjson resulting from Stanford to csv."
      
      started <- Sys.time()
      filenames <- Sys.glob(sprintf("%s/*.ndjson", file.path(.self$dir, "ndjson")))
      JsonParser <- NDJSON$new(
        destfile = file.path(.self$dir, targetDir, "tokenstream.csv"),
        logfile = file.path(.self$dir, targetDir, "parsingerrors.log")
        )
      JsonParser$processFiles(filenames)
      .self$updateProcessingTime(started = started, call = "xmlToDT")
      
    },
    
    addCposToMetadataTable = function(){
      
      .self$tokenstream[, cpos := 0:(nrow(.self$tokenstream) - 1)]
      grpn = uniqueN(.self$tokenstream[["chunk"]])
      pb <- txtProgressBar(min = 0, max = grpn, style = 3)
      cposDT <- .self$tokenstream[
        ,{
          setTxtProgressBar(pb, .GRP);
          list(cpos_left = min(.SD[["cpos"]]), cpos_right = max(.SD[["cpos"]]))
        }, by = "chunk"
        ]
      close(pb)
      setkeyv(cposDT, cols = "chunk")
      
      # dtMetadata <- data.table::fread("~/Lab/tmp/metadata.csv")
      setkeyv(.self$metadata, cols = "chunk")
      enhancedMetadataTable <- .self$metadata[cposDT]
      .self$metadata <- enhancedMetadataTable
      
    },
    
    encodePAttributes = function(corpus, cols = "word"){
      
      polmineR::encode(.self$tokenstream[[cols[1]]], corpus = corpus, pAttribute = cols[1], encoding = "UTF-8")
      if (length(cols) > 1){
        for (col in cols[2:length(cols)]){
          polmineR::encode(.self$tokenstream[[col]], corpus = corpus, pAttribute = col)
        }
      }
    },
    
    encodeSAttributes = function(corpus, cols = character()){
      
      for (col in cols){
        message("Encoding s-attribute: ", col)
        dtEnc <- .self$metadata[, c("cpos_left", "cpos_right", col), with = FALSE]
        polmineR::encode(dtEnc, corpus = "FOO", sAttribute = col)
      }
      
    },
    
    updateProcessingTime = function(started, call){
      ended <- Sys.time()
      .self$time <- rbind(
        .self$time,
        data.frame(start = started, end = ended, elapsed = ended - started, row.names = call)
      )
      .self$time["all", "end"] <- ended
      .self$time["all", "elapsed"] <- ended - .self$time["all", "start"]
      invisible(.self$updateProcessingTime)
    },
    
    size = function(){
      what <- c("basetable", "metadata", "texttable", "tokenstream")
      L <- lapply(
        setNames(what, what),
        function(x) format(object.size(.self[[x]]), "MB")
        )
      as.data.frame(as.matrix(L))
    }
    
  )
)
