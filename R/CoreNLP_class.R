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
  
  # fields = list(
  #   
  #   tagger = "jobjRef",
  #   xmlifier = "jobjRef",
  #   jsonifier = "jobjRef",
  #   writer = "jobjRef",
  #   append = "logical",
  #   method = "character",
  #   colsToKeep = "character",
  #   destfile = "character",
  #   logfile = "character"
  #   
  # ),
  
  fields = c(
    
    "tagger",
    "xmlifier",
    "jsonifier",
    "writer",
    "append",
    "method",
    "colsToKeep",
    "destfile",
    "logfile"
    
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
      javaVersion <- rJava::.jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
      message("Status of the Java Virtual Machine: ", jvmStatus)
      message("Java version: ", javaVersion)
      
      if (as.numeric(gsub("^(\\d+\\.\\d+)\\..*?$", "\\1", javaVersion)) != 1.8){
        warning("java version is not 1.8, but ", javaVersion, ". This may violate CoreNLP requirements.")
      }
      
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
            rJava::.jnew("java.io.FileOutputStream",
                  rJava::.jnew("java.io.File", filename),
                  TRUE)
          )
        }
      }
      
    },
    
    annotationToXML = function(anno){
      doc <- rJava::.jcall(.self$xmlifier, "Lnu/xom/Document;", "annotationToDoc", anno, .self$tagger)
      xml <- rJava::.jcall(doc, "Ljava/lang/String;", "toXML")
      df <- coreNLP::getToken(coreNLP:::parseAnnoXML(xml))
      colnames(df) <- tolower(colnames(df))
      as.data.table(df[, colsToKeep])
    },
    
    annotationToJSON = function(anno, id = NULL){
      jsonString <- rJava::.jcall(.self$jsonifier, "Ljava/lang/String;", "print", anno)
      jsonString <- gsub("\\s+", " ", jsonString)
      if (!is.null(id)){
        stopifnot(is.numeric(id))
        jsonString <- sprintf('{"id": %d, %s', id, substr(jsonString, 2, nchar(jsonString)))
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
        .self$writer <- rJava::.jnew("java.io.PrintWriter", filename <- tempfile())
      }
      rJava::.jcall(.self$tagger, "V", "prettyPrint", anno, .self$writer)
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
    
    annotate = function(txt, id = NULL, purge = TRUE){
      if (purge) txt <- .self$purge(txt)
      anno <- rJava::.jcall(.self$tagger, "Ledu/stanford/nlp/pipeline/Annotation;", "process", txt)
      switch(.self$method,
             xml = .self$annotationToXML(anno),
             json = .self$annotationToJSON(anno, id = id),
             txt = .self$annotationToTXT(anno)
      )
    }
  )
)
