#' Encode CWB corpus.
#' 
#' @field corpus name of the CWB corpus
#' @field registryFile filename of the registry file
#' @field registryDir corpus registry, the directory where registry files are stored
#' @field dataDir directory with indexed corpus files
#' @field encoding encoding/charst of the CWB corpus 
#' @field tokenstream \code{data.table} with tokenstream, original word forms / tokens
#' are in column 1, part-of-speech-annotation (pos), lemmatization in further columns
#' @field metadata \code{data.table} with 
#' 
#' @param .Object input object
#' @param corpus the name of the CWB corpus
#' @param pAttributes columns of .Object with tokens (such as word/pos/lemma)
#' @param sAttribute a single s-attribute
#' @param sAttributes columns of .Object that will be encoded as structural attributes
#' @param registry path to the corpus registry
#' @param dataDir directory where to create directory for indexed corpus files
#' @param verbose logical, whether to be verbose
#' 
#' @export Encoder
#' @rdname Encoder
#' @importFrom data.table uniqueN setkeyv fread fwrite setorderv
#' @examples 
#' library(tm)
#' reut21578 <- system.file("texts", "crude", package = "tm")
#' reuters.tm <- VCorpus(DirSource(reut21578), list(reader = readReut21578XMLasPlain))
#' 
#' library(tidytext)
#' reuters.tibble <- tidy(reuters.tm)
#' reuters.tibble[["topics_cat"]] <- sapply(
#'   reuters.tibble[["topics_cat"]],
#'   function(x) paste(reuters.tibble[["topics_cat"]], collapse = "|")
#' )
#' reuters.tibble[["places"]] <- sapply(
#'   reuters.tibble[["places"]],
#'   function(x) paste(x, collapse = "|")
#' )
#' reuters.tidy <- unnest_tokens(
#'   reuters.tibble, output = "word", input = "text", to_lower = FALSE
#' )
#' 
#' Enc <- Encoder$new(corpus = "reuters")
#' Enc$tokenstream <- as.data.table(reuters.tidy[, c("id", "word")])
#' Enc$metadata <- as.data.table(reuters.tibble[,c("id", "topics_cat", "places", "language")])
#' 
#' Enc$encode(pAttributes = "word", sAttributes = c("id", "topics_cat", "places", "language"))
Encoder <- setRefClass(
  
  "Encoder",
  
  fields = list(
    
    corpus = "character",
    registryFile = "character",
    registryDir = "character",
    dataDir = "character",
    encoding = "character",
    
    tokenstream = "data.table",
    metadata = "data.table"
    
  ),
  
  methods = list(
    
    initialize = function(corpus, registryDir = Sys.getenv("CORPUS_REGISTRY"), dataDir = NULL, encoding = "latin1"){
      
      "Initialize an encoder. If the corpus does not yet exist and dataDir is not provided explicitly,
      the data directory will be guessed and suggested. If the corpus is already present, the
      data directory will be derived from the registry file."
      
      .self$corpus <- corpus
      
      if (file.exists(registryDir))
        if (file.info(registryDir)[["isdir"]] != TRUE)
          stop("registryDir is not a directory")
      .self$registryDir <- registryDir
      .self$registryFile <- file.path(registryDir, corpus)
      if (file.exists(.self$registryFile)){
        message("corpus already exists")
        rf <- polmineR::RegistryFile$new(corpus = .self$corpus, registry = .self$registryDir)
        .self$dataDir <- rf$getHome()
      } else {
        message("corpus does not yet exist")
        if (is.null(dataDir)){
          superDir <- dirname(.self$registryDir)
          potentialDataDir <- grep("index", list.files(superDir), value = TRUE, perl = TRUE)
          if (length(potentialDataDir) != 1) stop("no dataDir provided, no candidate found")
          .self$dataDir <- file.path(superDir, potentialDataDir, tolower(.self$corpus))
          message(sprintf("suggesting dataDir: %s (Y/N) ", .self$dataDir))
          feedback <- readline(prompt = "")
          if (feedback != "Y") stop("aborting")
          if (!file.exists(.self$dataDir)) dir.create(.self$dataDir)
        } else {
          if (!file.exists(.self$dataDir)) dir.create(.self$dataDir)
          .self$dataDir <- dataDir
        }
      }
      
      if (!encoding %in% c("ascii", paste("latin", 1:9, sep = ""), "utf8")){
        stop("encoding is required to be among ascii; latin1 to latin9; utf8")
      }
      .self$encoding <- encoding
    },
    
    getEncoding = function(x, verbose = TRUE){
      enc <- unique(Encoding(x))
      if (length(enc) == 1){
        if (enc == "unknown"){
          if (verbose) message("... encoding of the input vector is 'unknown', assuming it to be that of the locale")
          return( localeToCharset()[1] )
        } else {
          if (verbose) message("... encoding of the input vector is: ", enc)
          return(enc)
        }
      } else if (length(enc) == 2){
        if ("unknown" %in% enc){
          enc <- enc[-which(enc == "unknown")]
          if (verbose) message("... encoding of the input vector is: ", enc)
          return( enc )
        } else {
          stop("please check encoding of the input character vector - more than one encoding found")
        }
      } else {
        stop("please check encoding of the input character vector - more than one encoding found")
      }
    },
    
    
    encodeTokenStream = function(verbose = TRUE){
      
      "Create a new CWB corpus from a input vector."
      
      if (verbose) message("Creating new CWB indexed corpus ", corpus)
      
      if (!"word" %in% colnames(.self$tokenstream)) stop("column 'word' required to be in table 'tokenstream'")

      ### 
      
      if (any(grepl("^\\s*<.*?>\\s*$", .self$tokenstream[["word"]])))
        warning("there is markup in the character vector - cwb-encode will issue warnings")
      
      # adjust encoding, if necessary
      inputEncoding <- .self$getEncoding(.self$tokenstream[["word"]])
      if (inputEncoding != .self$encoding){
        .self$tokenstream[["word"]] <- iconv(.self$tokenstream[["word"]], from = inputEncoding, to = .self$encoding)
        Encoding(.self$tokenstream[["word"]]) <- .self$encoding
      }
      
      if (verbose) message("... writing token stream to disk")
      vrtTmpFile <- tempfile()
      data.table::fwrite(.self$tokenstream[,"word"], file = vrtTmpFile,
                         col.names = FALSE, quote = FALSE, showProgress = TRUE
                         )
      

      if (verbose) message("... running cwb-encode")
      cwbEncodeCmdVec <- c(
        "cwb-encode",
        "-d", .self$dataDir, 
        "-f", vrtTmpFile,
        "-R", .self$registryFile,
        "-c", .self$encoding
      )
      cwbEncodeCmd <- paste0(cwbEncodeCmdVec,collapse = " ")

      system(cwbEncodeCmd)
      .self$cwbMake()
      polmineR::use(dir = .self$registryDir)
    },
    
    encodePositionalAttribute = function(pAttribute, verbose = TRUE){
      
      "Add positional attribute to a corpus that already exists."
      
      # some checks
      if (nrow(.self$tokenstream) != polmineR::size(toupper(.self$corpus)))
        stop("Length of character vector must be identical with size of corpus - not TRUE")
      
      if (pAttribute %in% polmineR::pAttributes(toupper(corpus)))
        stop("pAttribute already exists")
      
      if (any(grepl("^\\s*<.*?$", .self$tokenstream[[pAttribute]])))
        warning("there is markup in the character vector - cwb-encode will issue warnings")
      
      # adjust encoding, if necessary
      inputEncoding <- .self$getEncoding(.self$tokenstream[[pAttribute]])
      if (inputEncoding != .self$encoding){
        .self$tokenstream[[pAttribute]] <- iconv(.self$tokenstream[[pAttribute]], from = inputEncoding, to = .self$encoding)
        Encoding(.self$tokenstream[[pAttribute]]) <- .self$encoding
      }
      

      if (verbose) message("... writing vector to disk for p-attribute ", pAttribute)
      vrtTmpFile <- tempfile()
      data.table::fwrite(
        .self$tokenstream[, pAttribute, with = FALSE], file = vrtTmpFile,
        col.names = FALSE, quote = FALSE, showProgress = interactive()
      )
      
      if (verbose) message("... calling cwb-encode")
      pAttrsOld <- polmineR::RegistryFile$new(toupper(corpus))$getPAttributes() # for checking later if anything is missing
      cwbEncodeCmdVec <- c(
        "cwb-encode",
        "-d", .self$dataDir, # directory with indexed corpus files
        "-f", vrtTmpFile,
        "-R", .self$registryFile,
        "-p", "-", "-P", pAttribute,
        "-c", .self$encoding
      )
      cwbEncodeCmd <- paste0(cwbEncodeCmdVec, collapse = " ")
      system(cwbEncodeCmd)
      
      # cwb-encode may drop attributes from registry file apart from the newly encoded one ... 
      missingAttrs <- pAttrsOld[!pAttrsOld %in% polmineR::RegistryFile$new(corpus)$getPAttributes()]
      for (attr in missingAttrs) polmineR::RegistryFile$new(corpus)$addPAttribute(attr)
      
      .self$cwbMake()
      polmineR::use(dir = .self$registryDir)
    },
    
    encodeStructuralAttribute = function(sAttribute, verbose = TRUE){
      
      "Add a structural attribute to a corpus from a data.table with three columns:
      The left corpus position, the right corpus position and the value of a s-attribute
      that will be encoded."
      
      tab <- .self$metadata[, c("cpos_left", "cpos_right", sAttribute), with = FALSE]
      setorderv(tab, cols = "cpos_left", order = 1L)
      
      # adjust encoding, if necessary
      inputEncoding <- .self$getEncoding(as.character(tab[[sAttribute]]))
      if (inputEncoding != .self$encoding){
        tab[[sAttribute]] <- iconv(tab[[sAttribute]], from = inputEncoding, to = .self$encoding)
        Encoding(tab[[sAttribute]]) <- .self$encoding
      }

      if (verbose) message("... writing table to disk")
      tmp_file <- tempfile()
      data.table::fwrite(x = tab, file = tmp_file, quote = FALSE, sep = "\t", col.names = FALSE)
      
      if (verbose) message("... running cwb-s-encode")
      cmd <- c(
        "cwb-s-encode",
        "-d", .self$dataDir,
        "-f", tmp_file,
        "-V", sAttribute
      )
      
      system(paste(cmd, collapse = " "))
      
      if (!sAttribute %in% polmineR::sAttributes(toupper(.self$corpus))){
        if (verbose) message("... adding sAttribute to registry")
        R <- polmineR::RegistryFile$new(toupper(.self$corpus))
        R$read()
        R$addSAttribute(sAttribute)
        R$write()
      }
      
      polmineR::use(dir = .self$registryDir)
      
    },
    
    cwbMake = function(verbose = TRUE){
      
      "Run cwb-make."
      
      if (verbose) message("... running cwb-make")
      cwbMakeCmd <- paste0(
        c("cwb-make", "-V", .self$corpus, "-r", .self$registryDir),
        collapse = " "
      )
      system(cwbMakeCmd)
      polmineR::use(dir = .self$registryDir)
    },
    
    encode = function(pAttributes = "word", sAttributes = NULL, verbose = TRUE){
      
      "Encode CWB corpus from tables in fields metadata and tokenstream."
      
      .self$addCorpusPositionsToStructuralAttributesTable()
      
      .self$encodeTokenStream(verbose = verbose)
      
      # add other pAttributes than 'word'
      if (length(pAttributes > 1)){
        for (newAttribute in pAttributes[which(pAttributes != "word")]){
          .self$encodePositionalAttribute(pAttribute = newAttribute, verbose = verbose)
        }
      }
      
      for (sAttr in sAttributes){
        if (verbose) message("... encoding s-attribute ", sAttr)
        .self$encodeStructuralAttribute( sAttribute = sAttr)
      }
    },
    
    addCorpusPositionsToStructuralAttributesTable = function(verbose = TRUE){
      
      "Add columns cpos_left and cpos_right to table with structural attributes. A
      precondition is that a column 'id' is present in tables 'tokenstream' and 'metadata'."
      
      if (!"id" %in% colnames(.self$metadata)) stop("id column required")
      .self$tokenstream[, cpos := 0:(nrow(.self$tokenstream) - 1)]
      
      if (verbose) message("... adding corpus positions to table 'metadata'")
      grpn <- uniqueN(.self$tokenstream[["id"]])
      if (interactive()) pb <- txtProgressBar(min = 0, max = grpn, style = 3)
      cposDT <- .self$tokenstream[
        ,{
          if (interactive()) setTxtProgressBar(pb, .GRP);
          list(cpos_left = min(.SD[["cpos"]]), cpos_right = max(.SD[["cpos"]]))
        }, by = "id"
        ]
      if (interactive()) close(pb)
      setkeyv(cposDT, cols = "id")
      
      setkeyv(.self$metadata, cols = "id")
      .self$metadata <- .self$metadata[cposDT]
      
    }
    
  )
)
