#' Pipe for corpus preparation.
#' 
#' @field dir a pipe directory, different processing stages of the corpus will be kept in
#' subdirectories of this directory
#' @field time a data.frame with information that different processing stages have consumed
#' @field threads an integer, the number of cores to use
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
#' @importFrom data.table data.table rbindlist fread fwrite uniqueN
#' @export Pipe
Pipe <- setRefClass(
  
  "Pipe",
  
  fields = list(
    dir = "character",
    time = "data.frame",
    threads = "integer"
  ),
  
  methods = list(
    
    initialize = function(dir, threads = 1L){
      
      "Initialize a new Pipe object."
      
      if (!dir.exists(dir)) stop(" directory does not exist")
      .self$dir <- dir
      .self$time <- data.frame(
        start = Sys.time(), end = Sys.time(), elapsed = as.difftime(0, units = "secs"),
        row.names = "all"
        )
      .self$threads <- threads
    },
    
    preparePipeDirs = function(sourceDir, verbose = TRUE){
      
      "Create sourceDir, or the subdirectories of the pipeDir defined by the sourceDir
      character vector. If sourceDir already exists, files that are already present
      are deleted."
      
      for (subdir in sourceDir){
        neededDir <- file.path(.self$dir, subdir)
        if (!file.exists(neededDir)){
          dir.create(neededDir)
        } else {
          filesToDelete <- list.files(path = file.path(.self$dir, subdir), full.names = TRUE)
          lapply(filesToDelete, file.remove)
        }
      }
    },
    
    getFiles = function(sourceDir, targetDir, ...){
      
      "Get/copy Files from several directories to the subdirectory of the pipeDir
      defined by 'targetDir'."
      
      .getFiles(sourceDir = sourceDir, targetDir = file.path(.self$dir, targetDir), ...)
      
    },
    
    xmlToDT = function(sourceDir = "xml", targetDir = "tsv", metadata, verbose = TRUE){
      
      "Extract text and metadata from XML documents, and write resulting 'basetable' as
      tsv file to subdirectory specified by targetDir."
      
      started <- Sys.time()
      
      filenames <- list.files(file.path(.self$dir, sourceDir), full.names = TRUE)
      if (verbose) message("... parsing xml files in subdirectory ", sourceDir)
      dtList <- pbapply::pblapply(
        filenames,
        function(x) ctk::xmlToDT(x, meta = metadata), cl = .self$threads
        )
      dt <- rbindlist(dtList, fill = TRUE)
      rm(dtList)
      dt[, id := 1:nrow(dt)]
      if (!is.null(targetDir)){
        if (verbose) message("... writing basetable.tsv to subdirectory ", targetDir)
        data.table::fwrite(dt, file = file.path(.self$dir, targetDir, "basetable.tsv"))
      }
      .self$updateProcessingTime(started = started, call = "xmlToDT")
      invisible(dt)
    },
    
    makeMetadataTable = function(sourceDir = "tsv", targetDir = "tsv", verbose = TRUE){
      
      "Dissect file basetable.tsv in sourceDir into 'texttable' and 'metadata' as more memory efficient ways
      for keeping the data. If targetDir is not NULL, the resulting tables will be
      stored as tsv files in the respective subdirectory of the pipe directory."
      
      if (verbose) message("... reading in basetable")
      basetable <- data.table::fread(file.path(.self$dir, sourceDir, "basetable.tsv"))
      message("... writing metadata table")
      metadata <- basetable[, text := NULL]
      if (!is.null(targetDir)) data.table::fwrite(
        metadata,
        file = file.path(.self$dir, targetDir, "metadata.tsv"),
        quote = TRUE
        )
      invisible(metadata)
    },
    
    makePlaintextTable = function(sourceDir = "tsv", targetDir = "tsv", verbose = TRUE){
      
      "Dissect basetable into 'texttable' and 'metadata' as more memory efficient ways
      for keeping the data. If targetDir is not NULL, the resulting tables will be
      stored as tsv files in the respective subdirectory of the pipe directory."
      
      if (verbose) message("... reading in basetable")
      basetable <- data.table::fread(file.path(.self$dir, sourceDir, "basetable.tsv"))
      message("... extracting table with pain text and ids")
      texttable <- basetable[, c("id", "text"), with = TRUE]
      data.table::fwrite(texttable, file = file.path(.self$dir, targetDir, "texttable.tsv"))
      invisible(texttable)
    },
    
    
    corenlp = function(sourceDir = "tsv", targetDir = "ndjson", preclean = TRUE, verbose = TRUE){
      
      "Use Stanford CoreNLP to annotate . "
      
      if (verbose) message("... reading in texttable.tsv")
      texttable <- data.table::fread(file.path(.self$dir, sourceDir, "texttable.tsv"), showProgress = interactive())
      
      if (preclean){
        if (verbose) message("... removing characters not digested properly by StanfordCoreNLP")
        replacements <- list(
          c("a<`", "à"), c("e<'", "é"), c("o<\\^", "\u00F4"), c("<vs", "s"),
          c("<'c", "c"), c("s<v", "\u0161"), c("a<'", "á"), c("<vs", "\u0161"),
          c("<i<'", "í"), c("e<`", "è"), c("o<'", "ó"), c("z<v", "ž"), c("c<'", "ć"),
          c("<vz", "ž")
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
      if (.self$threads == 1){
        Annotator <- CoreNLP$new(
          method = "json",
          filename = file.path(.self$dir, targetDir, "annotation.ndjson"),
          stanfordDir = getOption("ctk.stanfordDir"),
          propertiesFile = getOption("ctk.propertiesFile")
        )
        dummy <- pbapply::pblapply(
          1:nrow(texttable),
          function(i) Annotator$annotate(texttable[["text"]][i], id = i)
        )
      } else if (.self$threads > 1){
        
        if (rJava:::.check.JVM()){
          message("JVM already up and running - parallelisation very likely to fail!")
        }
        if (Sys.getenv("RSTUDIO") == "1"){
          warning("for some unknown reason, parallelization does not work when RStudio is running")
        }
        chunks <- text2vec::split_into(1:nrow(texttable), n = .self$threads)
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
            lapply(chunks[[i]], function(j) Annotator$annotate(texttable[["text"]][j], id = j))
            return( outfiles[i] )
          }
          , mc.cores = .self$threads
        )
      }

    },
    
    ndjsonToDf = function(sourceDir = "ndjson", targetDir = "tsv"){
      
      "Convert ndjson resulting from Stanford to tsv."
      
      started <- Sys.time()
      filenames <- Sys.glob(sprintf("%s/*.ndjson", file.path(.self$dir, "ndjson")))
      message("... number of files to process: ", length(filenames))
      destfile <- file.path(.self$dir, targetDir, "tokenstream.tsv")
      message("... destfile: ", destfile)
      logfile <- file.path(.self$dir, targetDir, "parsingerrors.log")
      message("... logfile: ", logfile)
      JsonParser <- NDJSON$new(destfile = destfile, logfile = logfile)
      JsonParser$processFiles(filenames)
      .self$updateProcessingTime(started = started, call = "xmlToDT")
      
    },
    
    treetagger = function(sourceDir = "tok", targetDir = "vrt", lang = "de", ...){
      
      "Annotate all files in sourceDir using treetagger, and save results to targetDir."
      
      dirApply(
        f = .treetagger,
        sourceDir = file.path(.self$dir, sourceDir),
        targetDir=file.path(.self$dir, targetDir),
        param = list(lang = lang),
        ...
      )
    },
    
    addTreetaggerLemmatization = function(sourceDir = "tsv", targetDir = "tsv", lang = "de", verbose = TRUE){
      
      "The method will look for a file 'tokenstream.tsv' in the subdirectory of the pipeDir
      specified by sourceDir. To use the treetagger, a temporary file is created (tokenstream
      only) and annotated. The result is read in again, added to the original table and saved
      to an updated file tokenstream.tsv in the targetDir. If sourceDir and targetDir are
      identical, the original file is overwritten."
      
      
      if (verbose) message("... reading in tokenstream.tsv")
      tokenstreamDT <-  data.table::fread(file.path(P$dir, sourceDir, "tokenstream.tsv"), showProgress = interactive())
      tokenstreamDT <- tokenstreamDT[!is.na(tokenstreamDT[["word"]])] # NAs cause problems!
      
      
      if (verbose) message("... writing column with tokens to temporary file")
      tmpdir <- tempdir()
      data.table::fwrite(
        tokenstream[, "word", with = TRUE],
        file = file.path(tmpdir, "tokenstream.tok"),
        col.names = FALSE,
        quote = FALSE, showProgress = TRUE
      )
      
      if (verbose) message("... run treetagger")
      .treetagger(sourceDir = tmpdir, targetdir = tmpdir, filename = "tokenstream.tok", param = list(lang = lang))
      
      if (verbose) message("... read in treetagger output and supplement tokenstream data.table")
      treetaggerOutput <- data.table::fread(
        file.path(tmpdir, "tokenstream.vrt"), sep = "\t", col.names = c("word", "pos", "lemma"),
        showProgress = interactive()
      )
      tokenstreamDT[, lemma := c("im", treetaggerOutput[["lemma"]]) ]
      tokenstreamDT[, lemma := gsub("^<unknown>$", "#unknown#", tokenstreamDT[["lemma"]])]
      
      if (verbose) message("... write result back to tokenstream.tsv")
      data.table::fwrite(tokenstreamDT, file = file.path(.self$dir, targetDir, "tokenstream.tsv"), sep = "\t")
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
