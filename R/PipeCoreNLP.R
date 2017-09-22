#' Pipe using Stanford CoreNLP.
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{$xmlToBasetable(sourceDir = "xml", targetDir = "csv", metadata)}}{Extract text and metadata from XML documents, and keep the resulting
#'   'basetable' in the respective slot of the Pipe object.}
#'   \item{\code{$dissectBasetable(targetDir = "csv")}}{Dissect basetable into 'texttable' and 'metadata'
#'   as more memory efficient ways for keeping the data. If targetDir is not NULL, the resulting tables
#'    will be stored as csv files in the respective subdirectory of the pipe directory.}
#'   \item{\code{$corenlp(sourceDir = "csv", targetDir = "ndjson")}}{Use Stanford CoreNLP for annotation.}
#'   \item{\code{$ndjsonToDf(sourceDir = "ndjson", targetDir = "csv")}}{Convert CoreNLP json output to csv.}
#' }
#' 
#' @field dir a pipe directory, different processing stages of the corpus will be kept in
#' subdirectories of this directory
#' @field time a data.frame with information that different processing stages have consumed
#' @field threads an integer, the number of cores to use
#' 
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
#' @importFrom data.table data.table rbindlist fread fwrite uniqueN
#' @export Pipe
#' @importFrom R6 R6Class
#' @rdname PipeCoreNLP
PipeCoreNLP <- R6::R6Class(
  
  "PipeCoreNLP",
  inherit = Pipe,
  
  public = list(
    
    basetable = NULL, # "data.table",
    metadata = NULL, # "data.table",
    texttable = NULL, # "data.table",
    tokenstream = NULL, # "data.table",
    

    xmlToBasetable = function(sourceDir = "xml", targetDir = "csv", metadata){
      started <- Sys.time()
      
      filenames <- list.files(file.path(self$dir, sourceDir), full.names = TRUE)
      dtList <- pbapply::pblapply(
        filenames,
        function(x) ctk::xmlToDT(x, meta = metadata), cl = self$threads
      )
      self$basetable <- rbindlist(dtList, fill = TRUE)
      rm(dtList)
      self$basetable[, id := 1:nrow(P$basetable)]
      
      self$updateProcessingTime(started = started, call = "xmlToDT")
      invisible(self$basetable)
    },
    
    dissectBasetable = function(targetDir = "csv"){
      message("... extracting texttable")
      self$texttable <- self$basetable[, c("id", "text"), with = TRUE]
      if (!is.null(targetDir)) data.table::fwrite(
        self$texttable,
        file = file.path(self$dir, targetDir, "texttable.csv")
      )
      
      message("... extracting metadata")
      self$metadata <- self$basetable[, text := NULL]
      if (!is.null(targetDir)) data.table::fwrite(
        self$metadata,
        file = file.path(self$dir, targetDir, "metadata.csv"),
        quote = TRUE
      )
    },
    
    corenlp = function(sourceDir = "csv", targetDir = "ndjson"){
      
      if (nrow(self$texttable) == 0){
        stop("texttable needs to be present in the respective slot, but is not available")
      }
      stopifnot(c("id", "text") %in% colnames(self$texttable))
      started <- Sys.time()
      if (self$threads == 1){
        Annotator <- CoreNLP$new(
          method = "json",
          filename = file.path(self$dir, targetDir, "annotation.ndjson"),
          stanfordDir = self$stanfordDir, propertiesFile = self$propertiesFile
        )
        dummy <- pbapply::pblapply(
          1:nrow(self$texttable),
          function(i) Annotator$annotate(self$texttable[["text"]][i], id = i)
        )
      } else if (self$threads > 1){
        
        if (rJava:::.check.JVM()){
          message("JVM already up and running - parallelisation very likely to fail!")
        } else {
          message("")
        }
        if (Sys.getenv("RSTUDIO") == "1"){
          warning("for some unknown reason, parallelization does not work when RStudio is running")
        }
        chunks <- text2vec::split_into(1:nrow(self$texttable), n = self$threads)
        outfiles <- sprintf(
          file.path(self$dir, targetDir, "corenlp_%d.ndjson"),
          1:self$threads
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
            lapply(chunks[[i]], function(j) Annotator$annotate(self$texttable[["text"]][j], id = j))
            return( outfiles[i] )
          }
          , mc.cores = self$threads
        )
      }
      
    },
    
    ndjsonToDf = function(sourceDir = "ndjson", targetDir = "csv"){
      started <- Sys.time()
      filenames <- Sys.glob(sprintf("%s/*.ndjson", file.path(self$dir, "ndjson")))
      message("... number of files to process: ", length(filenames))
      destfile <- file.path(self$dir, targetDir, "tokenstream.csv")
      message("... destfile: ", destfile)
      logfile <- file.path(self$dir, targetDir, "parsingerrors.log")
      message("... logfile: ", logfile)
      JsonParser <- NDJSON$new(destfile = destfile, logfile = logfile)
      JsonParser$processFiles(filenames)
      self$updateProcessingTime(started = started, call = "xmlToDT")
      
    },
    
    addCposToMetadataTable = function(){
      
      self$tokenstream[, cpos := 0:(nrow(self$tokenstream) - 1)]
      grpn <- uniqueN(self$tokenstream[["id"]])
      pb <- txtProgressBar(min = 0, max = grpn, style = 3)
      cposDT <- self$tokenstream[
        ,{
          setTxtProgressBar(pb, .GRP);
          list(cpos_left = min(.SD[["cpos"]]), cpos_right = max(.SD[["cpos"]]))
        }, by = "id"
        ]
      close(pb)
      setkeyv(cposDT, cols = "id")
      
      # dtMetadata <- data.table::fread("~/Lab/tmp/metadata.csv")
      setkeyv(self$metadata, cols = "id")
      enhancedMetadataTable <- self$metadata[cposDT]
      self$metadata <- enhancedMetadataTable
    },
    
    encodePAttributes = function(corpus, cols = "word"){
      
      polmineR::encode(self$tokenstream[[cols[1]]], corpus = corpus, pAttribute = cols[1], encoding = "UTF-8")
      if (length(cols) > 1){
        for (col in cols[2:length(cols)]){
          polmineR::encode(self$tokenstream[[col]], corpus = corpus, pAttribute = col)
        }
      }
    },
    
    encodeSAttributes = function(corpus, cols = character()){
      
      for (col in cols){
        message("Encoding s-attribute: ", col)
        dtEnc <- self$metadata[, c("cpos_left", "cpos_right", col), with = FALSE]
        polmineR::encode(dtEnc, corpus = "FOO", sAttribute = col)
      }
      
    },
    
    updateProcessingTime = function(started, call){
      ended <- Sys.time()
      self$time <- rbind(
        self$time,
        data.frame(start = started, end = ended, elapsed = ended - started, row.names = call)
      )
      self$time["all", "end"] <- ended
      self$time["all", "elapsed"] <- ended - self$time["all", "start"]
      invisible(self$updateProcessingTime)
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
