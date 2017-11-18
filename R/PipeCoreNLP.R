#' Pipe using Stanford CoreNLP.
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{$corenlp(sourceDir = "tsv", targetDir = "ndjson")}}{Use Stanford CoreNLP for annotation.}
#'   \item{\code{$ndjsonToDf(sourceDir = "ndjson", targetDir = "tsv")}}{Convert CoreNLP json output to csv.}
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
#' @importFrom text2vec split_into
#' @importFrom data.table data.table rbindlist fread fwrite uniqueN
#' @export PipeCoreNLP
#' @importFrom R6 R6Class
#' @rdname PipeCoreNLP
PipeCoreNLP <- R6::R6Class(
  
  "PipeCoreNLP",
  inherit = Pipe,
  
  public = list(
    
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
          function(i) Annotator$annotate(texttable[["text"]][i], id = texttable[i][["id"]])
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
            lapply(chunks[[i]], function(j) Annotator$annotate(texttable[["text"]][j], id = texttable[j][["id"]]))
            return( outfiles[i] )
          }
          , mc.cores = self$threads
        )
      }
    },
    
    ndjsonToDf = function(sourceDir = "ndjson", targetDir = "tsv"){
      started <- Sys.time()
      filenames <- Sys.glob(sprintf("%s/*.ndjson", file.path(self$dir, "ndjson")))
      # ensure that filenames are processed in the correct order
      if (all(grepl("\\d+", basename(filenames)))){
        filenames <- filenames[order(as.integer(gsub("^.*?(\\d+).*?$", "\\1", basename(filenames))))]
      }
      message("... number of files to process: ", length(filenames))
      destfile <- file.path(self$dir, targetDir, "tokenstream.tsv")
      logfile <- file.path(self$dir, targetDir, "parsingerrors.log")
      if (self$threads == 1){
        JsonParser <- NDJSON$new(destfile = destfile, logfile = logfile)
        JsonParser$processFiles(filenames)
      } else {
        filenameList <- text2vec::split_into(filenames, n = self$threads)
        tmpdir <- tempdir()
        # removing potentially remaining files in tmpdir is necessary, because the NDJSON
        # class would just add new output to an already existing file
        unlink(Sys.glob(sprintf("%s/*.tok", tmpdir)))
        unlink(Sys.glob(sprintf("%s/*.log", tmpdir)))
        message("... using temporary directory: ", tmpdir)
        dtList <- parallel::mclapply(
          1:length(filenameList),
          function(i){
            tmpdestfile <- file.path(tmpdir, paste(i, "tok", sep = "."))
            tmplogfile <- file.path(tmpdir, paste(i, "log", sep = "."))
            JsonParser <- NDJSON$new(destfile = tmpdestfile, logfile = tmplogfile)
            JsonParser$processFiles(filenameList[[i]])
            data.table::fread(tmpdestfile, sep = "\t", header = TRUE, showProgress = FALSE)
          },
          mc.cores = self$threads
        )
        dt <- data.table::rbindlist(dtList)
        setkeyv(dt, cols = "id")
        setorderv(dt, cols = "id")
        data.table::fwrite(
          dt, file = destfile,
          sep = "\t", row.names = FALSE, col.names = TRUE
          )
        logList <- lapply(
          file.path(tmpdir, paste(1:length(filenameList), "log", sep = ".")),
          function(x) if (file.exists(x)) readLines(x, warn = FALSE) else NULL
        )
        writeLines(unlist(logList), con = logfile)
      }
      self$updateProcessingTime(started = started, call = "xmlToDT")
    }
  )
)
