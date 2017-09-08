#' Class for using Stanford CoreNLP
#' 
#' 
#' @field tagger ...
#' @field xmlifier ...
#' @field jsonifier ...
#' @field writer ...
#' @field append ...
#' @field method ...
#' @field colsToKeep ...
#' @field destfile ...
#' @field logfile ...
#' 
#' @param filename if filename is not NULL (default), the object will be initialized with
#' a FileWriter, and new annotations will be appended
#' @param colsToKeep character vector with names of columens of the output data.table
#' 
#' @export CoreNLP
#' @exportClass CoreNLP
#' @rdname CoreNLP
#' @importFrom jsonlite fromJSON
#' @importFrom stringi stri_match
CoreNLP <- setRefClass(
  
  "CoreNLP",
  
  fields = list(
    
    tagger = "jobjRef",
    xmlifier = "jobjRef",
    jsonifier = "jobjRef",
    writer = "jobjRef",
    append = "logical",
    method = "character",
    colsToKeep = "character",
    destfile = "character",
    logfile = "character"
    
  ),
  
  methods = list(
    
    initialize = function(
      stanfordDir = getOption("ctk.stanfordDir"),
      propertiesFile = getOption("ctk.propertiesFile"), 
      method = c("txt", "json", "xml"),
      colsToKeep = c("sentence", "id", "token", "pos", "ner"),
      filename = NULL
    ){
      
      .self$colsToKeep <- colsToKeep
      
      jvmStatus <- rJava::.jinit(force.init = TRUE) # does it harm when called again?
      message("Status of the Java Virtual Machine: ", jvmStatus)
      
      # add stanford jars to classpath
      if (is.null(stanfordDir)){
        stanfordDir <- file.path(
          system.file("extdata", package = "coreNLP"),
          "stanford-corenlp-full-2015-12-09"
        )
      }
      stanfordPath <- Sys.glob(paste0(stanfordDir,"/*.jar"))
      rJava::.jaddClassPath(stanfordPath)
      
      .self$method <- method
      if (.self$method == "xml") .self$xmlifier <- rJava::.jnew("edu.stanford.nlp.pipeline.XMLOutputter")
      if (.self$method == "json") .self$jsonifier <- rJava::.jnew("edu.stanford.nlp.pipeline.JSONOutputter")
      
      if (is.null(propertiesFile)){
        propertiesFile <- file.path(
          system.file(package = "coreNLP", "extdata"),
          "StanfordCoreNLP-german.properties"
        )
      }
      rJava::.jaddClassPath(dirname(propertiesFile))
      
      .self$tagger <- rJava::.jnew(
        "edu.stanford.nlp.pipeline.StanfordCoreNLP",
        basename(propertiesFile)
      )
      
      if (is.null(filename)){
        .self$append <- FALSE
      } else {
        .self$append <- TRUE
        .self$destfile <- filename
        if (method == "txt"){
          .self$writer <- new(
            J("java.io.PrintWriter"),
            .jnew("java.io.FileOutputStream",
                  .jnew("java.io.File", filename),
                  TRUE)
          )
        }
      }
      
    },
    
    annotationToXML = function(anno){
      doc <- .jcall(.self$xmlifier, "Lnu/xom/Document;", "annotationToDoc", anno, .self$tagger)
      xml <- .jcall(doc, "Ljava/lang/String;", "toXML")
      df <- coreNLP::getToken(coreNLP:::parseAnnoXML(xml))
      colnames(df) <- tolower(colnames(df))
      as.data.table(df[, colsToKeep])
    },
    
    annotationToJSON = function(anno, chunk = NULL){
      jsonString <- .jcall(.self$jsonifier, "Ljava/lang/String;", "print", anno)
      jsonString <- gsub("\\s+", " ", jsonString)
      if (!is.null(chunk)){
        stopifnot(is.numeric(chunk))
        jsonString <- sprintf('{"chunk": %d, %s', chunk, substr(jsonString, 2, nchar(jsonString)))
      }
      cat(jsonString, "\n", file = .self$destfile, append = .self$append)
      if (.self$append == FALSE){
        return(jsonString)
      } else {
        return( NULL )
      }
    },
    
    annotationToTXT = function(anno){
      if (.self$append == FALSE){
        .self$writer <- .jnew("java.io.PrintWriter", filename <- tempfile())
      }
      .jcall(.self$tagger, "V", "prettyPrint", anno, .self$writer)
      # .jmethods(writer, "V", "close")
      if (.self$append == FALSE){
        return( .self$parsePrettyPrint(filename) )
      } else {
        return( NULL )
      }
    },
    
    purge = function(x){
      replacements <- list(
        c("\u202F", ""), # narrow no-break space
        c("\uFFFD", "")
      )
      for (i in 1:length(replacements)){
        x <- gsub(replacements[[i]][1], replacements[[i]][2], x)
      }
      x
    },
    
    annotate = function(txt, chunk = NULL, purge = TRUE){
      if (purge) txt <- .self$purge(txt)
      anno <- rJava::.jcall(.self$tagger, "Ledu/stanford/nlp/pipeline/Annotation;", "process", txt)
      switch(.self$method,
             xml = .self$annotationToXML(anno),
             json = .self$annotationToJSON(anno, chunk = chunk),
             txt = .self$annotationToTXT(anno)
      )
    }
  )
)