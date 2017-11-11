#' Pipe using Stanford CoreNLP.
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{$corenlp(sourceDir = "csv", targetDir = "ndjson")}}{Use Stanford CoreNLP for annotation.}
#'   \item{\code{$ndjsonToDf(sourceDir = "ndjson", targetDir = "csv")}}{Convert CoreNLP json output to csv.}
#' }
#' 
#' @field dir a pipe directory, different processing stages of the corpus will be kept in
#' subdirectories of this directory
#' @field time a data.frame with information that different processing stages have consumed
#' @field threads an integer, the number of cores to use
#' 
#' #' @section Arguments:
#' \describe{
#'   \item{metadata}{a named list with xpath expressions that is passed into the
#'   xmlToDT function to extract metadata from XML documents}
#'   \item{sourceDir}{a subdirectory of the pipeDir where files to be processed 
#'   are located}
#'   \item{targetDir}{a subdirectory of the pipeDir where processed output is
#'   stored}
#'   \item{cols}{columns that will serve as input to generate p- and s-attributes}
#'   \item{corpus}{name of the CWB corpus to create}
#' }
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
    
    
    corenlp = function(sourceDir = "tsv", targetDir = "ndjson", preclean = TRUE, verbose = TRUE){
      
      "Use Stanford CoreNLP to annotate . "
      
      if (verbose) message("... reading in texttable.tsv")
      texttable <- data.table::fread(file.path(self$dir, sourceDir, "texttable.tsv"), showProgress = interactive())
      
      if (preclean){
        if (verbose) message("... removing characters not digested properly by StanfordCoreNLP")
        replacements <- list(
          c("a<`", "\uE0"), c("e<'", "\uE9"), c("o<\\^", "\u00F4"), c("<vs", "s"),
          c("<'c", "c"), c("s<v", "\u0161"), c("a<'", "\uE1"), c("<vs", "\u0161"),
          c("<i<'", "\uED"), c("e<`", "\uE8"), c("o<'", "0xF3"), c("z<v", "\u17E"), c("c<'", "\u107"),
          c("<vz", "\u17E")
        )
        if (interactive()) pb <- txtProgressBar(min = 1, max = length(replacements), style = 3)
        for (i in 1:length(replacements)){
          if (interactive()) setTxtProgressBar(pb, i)
          texttable[, text := gsub(replacements[[i]][1], replacements[[i]][2], texttable[["text"]])]
        }
        if (interactive()) close(pb)
      }
      
      stopifnot(c("id", "text") %in% colnames(texttable))
      started <- Sys.time()
      if (self$threads == 1){
        Annotator <- CoreNLP$new(
          method = "json",
          filename = file.path(self$dir, targetDir, "annotation.ndjson"),
          stanfordDir = getOption("ctk.stanfordDir"),
          propertiesFile = getOption("ctk.propertiesFile")
        )
        dummy <- pbapply::pblapply(
          1:nrow(texttable),
          function(i) Annotator$annotate(texttable[["text"]][i], id = i)
        )
      } else if (self$threads > 1){
        
        if (rJava:::.check.JVM()){
          message("JVM already up and running - parallelisation very likely to fail!")
        }
        if (Sys.getenv("RSTUDIO") == "1"){
          warning("for some unknown reason, parallelization does not work when RStudio is running")
        }
        chunks <- text2vec::split_into(1:nrow(texttable), n = self$threads)
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
            lapply(chunks[[i]], function(j) Annotator$annotate(texttable[["text"]][j], id = j))
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
        function(x) format(object.size(self[[x]]), "MB")
      )
      as.data.frame(as.matrix(L))
    }
  )
)
