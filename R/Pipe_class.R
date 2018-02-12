#' @name Pipe
#' @title Pipe for corpus preparation.
#' @description The \code{Pipe} class offers a framework for corpus preparation
#'   and auxiliary tools. The methods of the class (wrappers for standard tools
#'   or helpers) use subdirectories of a pipe directory to take files and their
#'   content through the different stages of corpus preparation. To use Stanford
#'   CoreNLP, the class is extended by the \code{PipeCoreNLP} class.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Fiels} sections.
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{$new(dir, threads = 1L)}}{Initialize new \code{Pipe} object.}
#'   \item{\code{$summary()}}{Return \code{data.frame} with number of files in
#'   the subdirectories of the pipe directory.}
#'   \item{\code{$preparePipeDir(subdirs = character(), delete = FALSE, verbose = 
#'   TRUE)}}{Create subdirectories provided by \code{subdirs} in the pipe 
#'   directory, and delete existing files, if \code{delete} is \code{TRUE}.}
#'   \item{\code{$getFiles(sourceDir, targetDir, ...)}}{Copy Files from 
#'   directories indicated by to the subdirectory of the pipe directory defined 
#'   by \code{targetDir}. See documentation for helper function \code{getFiles} 
#'   for options available through \code{...}.}
#'   \item{\code{$getMissingFiles(sourceDir, targetDir, ignore.extensions =
#'   TRUE)}}{ Identify files that are present in \code{sourceDir}, but not in 
#'   \code{targetDir}. If \code{ignore.extensions} is \code{TRUE}, file
#'   extensions are removed before comparing filenames.}
#'   \item{\code{$rsync()}}{Prepare rsync command that can be used to
#'   synchronize pipe directory with a remote storage.}
#'   \item{\code{mergeXMLFiles(sourceDir, targetDir, regex, rootElement,
#'   rootAttrs, mc=FALSE, verbose=TRUE, ...)}}{Merge files into single XML
#'   documents for faster processing during later stages of the pipe.}
#'   \item{\code{$validate(sourceDir, targetDir = NULL, dtd = NULL, 
#'   ...)}}{Validate that all XML files in \code{sourceDir} are valid XML files.
#'   if \code{dtd} is provided, check against a DTD.}
#'   \item{\code{$getAttributeValues(sourceDir, pattern, element, attrs, unique 
#'   = TRUE, mc = FALSE, progress = TRUE)}}{Get values of XML attributes defined
#'   by \code{attrs} for the element defined by \code{element}.}
#'   \item{\code{$consolidate(sourceDir, targetDir, consolidation, element, 
#'   attribute, ...)}}{Perform replacements for XML attributes as provided by 
#'   character vector \code{consolidation}. (Further documentation is needed!)}
#'   \item{\code{$xmlToDT(sourceDir = "xml", targetDir = "tsv", 
#'   metadata)}}{Extract text and metadata from XML documents, and write resulting
#'   'basetable' as tsv file to subdirectory specified by targetDir. The basetable
#'   is returned invisibly.}
#'   \item{\code{addTreetaggerLemmatization(sourceDir = "tsv", targetDir = 
#'   "tsv", lang = "de", verbose = TRUE)}}{The method will look for a file
#'   'tokenstream.tsv' in the subdirectory of the pipeDir specified by
#'   sourceDir. To use the treetagger, a temporary file is created (tokenstream
#'   only) and annotated. The result is read in again, added to the original
#'   table and saved to an updated file tokenstream.tsv in the targetDir. If
#'   sourceDir and targetDir are identical, the original file is overwritten.}
#'   \item{\code{$makeMetadataTable(sourceDir = "tsv", targetDir = "tsv",
#'   verbose = TRUE)}}{Dissect file basetable.tsv in sourceDir into 'texttable'
#'   and 'metadata' as more memory efficient ways for keeping the data. If
#'   targetDir is not NULL, the resulting tables will be stored as tsv files in
#'   the respective subdirectory of the pipe directory.}
#'   \item{\code{$makePlaintextTable(sourceDir = "tsv", targetDir = "tsv", 
#'   verbose = TRUE)}}{Dissect basetable into 'texttable' and 'metadata' as more
#'   memory efficient ways for keeping the data. If targetDir is not NULL, the
#'   resulting tables will be stored as tsv files in the respective subdirectory
#'   of the pipe directory.}
#'   \item{\code{$xslt(sourceDir, targetDir, xslFile, ...)}}{Perform XSL
#'   transformation.}
#'   \item{\code{$subset(sourceDir, targetDir, sample = NULL, files =
#'   NULL)}}{Generate a subset of files in the \code{sourceDir}, copying a
#'   choice of files to \code{targetDir}. If \code{files} is a character vector
#'   with filenames, it will be these files that are copied. If \code{sample} is
#'   provided (a number), a random sample is drawn.}
#'   \item{\code{$recode(sourceDir, targetDir, from = "UTF-8", to = 
#'   "ISO-8859-1", xml = FALSE, log = FALSE, ...)}}{Recode files in 
#'   \code{sourceDir}, writing results to \code{targetDir}. See documentation 
#'   for worker function \code{recode} for options that are available.}
#'   \item{\code{$replaceInvalidCharacters(sourceDir, targetDir, xml = FALSE,
#'   ...)}}{Replace characters that are known to cause problems.} 
#'   \item{\code{$findAndReplace(sourceDir, targetDir, replacements, encoding,
#'   ...)}}{Find matches for a regular expression and perform replacemet;
#'   \code{replacements} is a list of length 2 character vectors, which provide
#'   the regex and the replacement.}
#'   \item{\code{$tokenize(sourceDir, targetDir,
#'   with = "stanfordNLP", lang = "de", ...)}}{Tokenize files in
#'   \code{sourceDir}, and save results to \code{targetDir}. The result will be
#'   a verticalized format that can be used for the TreeTagger.} 
#'   \item{\code{$tokenizeSentences(sourceDir = "xml",targetDir="xmlAnno",
#'   targetElement = "p", para = FALSE, ...)}}{Use the NLTK sentence tokenizer.}
#'   \item{\code{$treetagger(sourceDir = "tok", targetDir = "vrt", lang = "de",
#'   ...)}}{Annotate all files in \code{sourceDir} using treetagger, and save
#'   results to \code{targetDir}.}
#'   \item{\code{$fix(sourceDir, targetDir,
#'   encoding = "UTF-8", replacements = list(), ...)}}{Check files in 
#'   \code{sourceDir} for potential hickups they may cause, and save output with
#'   error corrections to \code{targetDir}.} 
#'   \item{\code{$sAttributeList(sourceDir, sample = 100, ...)}}{Analyse
#'   structure of XML and return list describing this structure.} 
#'   \item{\code{$getNestedElements(sourceDir, corpus, element, max.embedding =
#'   NULL)}}{Helper methode to detect errors in XML documents where cwb-encode
#'   will throw an error because elements are nested.}
#' }
#' 
#' @field dir a pipe directory, different processing stages of the corpus will be kept in
#' subdirectories of this directory
#' @field time a data.frame with information that different processing stages have consumed
#' @field threads an integer, the number of cores to use
#'   
#' @section Arguments:
#' \describe{
#'   \item{dir}{the pipe directory}
#'   \item{threads}{an integer, the number of threads to use}
#'   \item{sourceDir}{a subdirectory of the pipeDir where files to be processed 
#'   reside}
#'   \item{targetDir}{a subdirectory of the pipeDir where processed output is
#'   stored}
#'   \item{ignore.extension}{logical, whether to remove file extensions before
#' checking whether files in \code{sourceDir} are present in \code{targetDir}}
#'   \item{corpus}{name of the CWB corpus to create}
#' }
#' 
#' @importFrom pbapply pblapply
#' @importFrom parallel mclapply
#' @importFrom data.table data.table rbindlist fread fwrite uniqueN
#' @export Pipe
#' @importFrom R6 R6Class
#' @rdname Pipe
Pipe <- R6::R6Class(
  
  "Pipe",
  
  public = list(
    
    dir = NULL, # "character",
    time = NULL, # "data.frame",
    threads = NULL, # "integer",
    basetable = NULL, # "data.table",
    metadata = NULL, # "data.table",
    texttable = NULL, # "data.table",
    tokenstream = NULL, # "data.table",

    initialize = function(dir, threads = 1L){
      if (!dir.exists(dir)) stop(" directory does not exist")
      self$dir <- dir
      self$time <- data.frame(
        start = Sys.time(), end = Sys.time(), elapsed = as.difftime(0, units = "secs"),
        row.names = "all"
      )
      self$threads <- threads
    },
    
    summary = function(){
      subdirs <- list.dirs(self$dir, full.names = FALSE)
      subdirs <- subdirs[which(subdirs != "")]
      subdirInfo <- lapply(
        setNames(subdirs, subdirs),
        function(subdir){
          dirFullPath <- file.path(self$dir, subdir)
          filenames <- list.files(dirFullPath, full.names = TRUE)
          infoDF <- file.info(filenames)
          data.frame(
            # subdir = subdir,
            files = length(filenames),
            first = if (length(filenames) > 0) min(infoDF[["atime"]]) else NA,
            last = if (length(filenames) > 0) last(infoDF[["atime"]]) else NA
          )
        }
      )
      df <- do.call(rbind, subdirInfo)
      df[["difftime"]] <- difftime(df$last, df$first, units = "auto")
    },
    
    preparePipeDir = function(subdirs = character(), delete = FALSE, verbose = TRUE){
      for (subdir in subdirs){
        neededDir <- file.path(self$dir, subdir)
        if (!file.exists(neededDir)){
          dir.create(neededDir)
        } else {
          if (delete){
            filesToDelete <- list.files(path = file.path(pipeDir, subdir), full.names = TRUE)
            if (length(filesToDelete) > 0){
              if (verbose) message("... deleting files in directory: ", neededDir)
              lapply(filesToDelete, file.remove)
            }
          }
        }
      }
    },
    
    getFiles = function(sourceDir, targetDir, ...){
      .getFiles(sourceDir = sourceDir, targetDir = file.path(self$dir, targetDir), ...)
    },
    
    getMissingFiles = function(sourceDir, targetDir, ignore.extensions = TRUE){
      filesInSourceDir <- list.files(file.path(self$dir, sourceDir))
      filesInTargetDir <- list.files(file.path(self$dir, targetDir))
      if (ignore.extensions){
        names(filesInSourceDir) <- filesInSourceDir
        filesInSourceDir <- gsub("^(.*?)\\..*?$", "\\1", filesInSourceDir)
        names(filesInTargetDir) <- filesInTargetDir
        filesInTargetDir <- gsub("^(.*?)\\..*?$", "\\1", filesInTargetDir)
        missingFiles <- filesInSourceDir[!filesInSourceDir %in% filesInTargetDir]
        return( names(missingFiles) )
      } else {
        missingFiles <- filesInSourceDir[!filesInSourceDir %in% filesInTargetDir]
        return( missingFiles)
      }
    },
    
    subset = function(sourceDir, targetDir, sample = NULL, files = NULL){
      if (is.null(files)){
        files <- list.files(file.path(self$dir, sourceDir))
      }
      if (!is.null(sample)){
        files <- sample(files, size=sample)
      }
      lapply(
        files,
        function(file){
          file.copy(
            from = file.path(self$dir, sourceDir, file),
            to = file.path(self$dir, targetDir, file)
          )
        })
    },
    
    rsync = function(){
      cmd <- c("rsync", "-avzbe", "ssh", self$dir, self$remoteDir)
      cmd <- paste(cmd, collapse = " ")
      print(cmd)
    },
    
    mergeXMLFiles = function(sourceDir, targetDir, regex, rootElement, rootAttrs, mc = FALSE, verbose = TRUE, ...){
      if (verbose) message("... getting files in sourceDir")
      files <- list.files(file.path(self$dir, sourceDir))
      matches <- unique(gsub(regex, "\\1", files))
      fileEnding <- unique(gsub("^.*(\\..*)$", "\\1", files))
      if (length(fileEnding) > 1) warning("... WARNING: various file endings present in this dir")
      newFiles <- paste(matches, fileEnding[1], sep="")
      newRegexSet <- sapply(matches, function(x) gsub("\\(.*\\)", x, regex))
      if (verbose) message("... grouping the files")
      filesByGroups <- lapply(newRegexSet, function(newRegex) grep(newRegex, files, value=TRUE) )
      lengthsFileSet <- lapply(
        1:length(filesByGroups),
        function(noFileSet){
          fileSet <- filesByGroups[[noFileSet]]
          nameFileSet <- names(filesByGroups)[noFileSet]
          if (verbose == TRUE) message("... processing group ", nameFileSet)
          fileName <- newFiles[[noFileSet]]
          newFile <- file.path(self$dir, targetDir, fileName)
          lengthFileSet <- lapply(
            c(1:length(fileSet)),
            function(i){
              input <- scan(
                file=file.path(self$dir, sourceDir, fileSet[i]),
                what = "character", sep = "\n", quiet = TRUE
              )
              posXmlDeclaration <- grep("^\\s*<\\?xml.*", input)
              if (i == 1) {
                rootAttrsChar <- paste(
                  "",
                  sapply(
                    1:length(rootAttrs),
                    function(x){ paste(names(rootAttrs[x]), '="', rootAttrs[x],'"', sep='') }
                  ),
                  collapse='')
                newRootNode <- paste('<', rootElement, ' group="', nameFileSet, '"', rootAttrsChar, '>',  sep='')
                output <- c(input[posXmlDeclaration], newRootNode, input[c((posXmlDeclaration+1):length(input))])
                cat(output, file=newFile, sep="\n")
              } else {
                cat(input[-posXmlDeclaration], file=newFile, sep="\n", append=TRUE)
              }
              return(setNames(length(fileSet), nameFileSet))
            })
          cat(paste('</', rootElement, '>', sep=""), file = newFile, sep = "\n", append=TRUE)
          return(lengthFileSet)
        })
      return(lengthsFileSet)
    },
    
    validate = function(sourceDir, targetDir = NULL, dtd = NULL, ...){
      if (!is.null(targetDir)) targetDir <- file.path(self$dir, targetDir)
      dirApply(
        f = .validateWorker, x = file.path(self$dir, sourceDir), y = targetDir,
        dtd = dtd, ... 
      )
    },
    
    xslt = function(sourceDir, targetDir, xslFile, ...){
      dirApply(
        f = .xsltWorker,
        sourceDir = file.path(self$dir, sourceDir),
        targetDir = file.path(self$dir, targetDir),
        param = list(xslFile=xslFile), 
        ...
      )       
    },
    
    recode = function(sourceDir, targetDir, from = "UTF-8", to = "ISO-8859-1", xml = FALSE, log = FALSE, ...){
      dirApply(
        f = .iconvWorker,
        sourceDir = file.path(self$dir, sourceDir),
        targetDir = file.path(self$dir, targetDir),
        param = list(from = from, to = to, xml = xml, log = log),
        ...
      )
    },
    
    replaceInvalidCharacters = function(sourceDir, targetDir, xml = FALSE, ...){
      if (xml == FALSE) {
        stopifnot(!is.null(sourceDir))
        dirApply(
          f = .replaceInTxtFile,
          sourceDir = file.path(self$dir, sourceDir),
          targetDir=file.path(self$dir, targetDir),
          param = list(), ...
        )
      } else if (xml == TRUE){
        stop("replacing invalid characters in XML files not yet implemented")
      }
    },
    
    findAndReplace = function(sourceDir, targetDir, replacements, encoding, ...){
      dirApply(
        f = .findAndReplace,
        sourceDir=file.path(self$dir, sourceDir),
        targetDir=file.path(self$dir, targetDir),
        param=list(replacements=replacements, encoding=encoding),
        ...
      )
    },
    
    xmlToDT = function(sourceDir = "xml", targetDir = "tsv", metadata, verbose = TRUE){
      started <- Sys.time()
      
      filenames <- list.files(file.path(self$dir, sourceDir), full.names = TRUE)
      if (verbose) message("... parsing xml files in subdirectory ", sourceDir)
      dtList <- pbapply::pblapply(
        filenames,
        function(x) ctk::xmlToDT(x, meta = metadata),
        cl = self$threads
      )
      dt <- rbindlist(dtList, fill = TRUE)
      rm(dtList)
      dt[, id := 1:nrow(dt)]
      if (!is.null(targetDir)){
        if (verbose) message("... writing basetable.tsv to subdirectory ", targetDir)
        data.table::fwrite(dt, file = file.path(self$dir, targetDir, "basetable.tsv"))
      }
      self$updateProcessingTime(started = started, call = "xmlToDT")
      invisible(dt)
    },
    
    
    makeMetadataTable = function(sourceDir = "tsv", targetDir = "tsv", verbose = TRUE){
      if (verbose) message("... reading in basetable")
      basetable <- data.table::fread(file.path(self$dir, sourceDir, "basetable.tsv"))
      message("... writing metadata table")
      metadata <- basetable[, text := NULL]
      if (!is.null(targetDir)) data.table::fwrite(
        metadata,
        file = file.path(self$dir, targetDir, "metadata.tsv"),
        quote = TRUE, showProgress = interactive()
      )
      invisible(metadata)
    },
    
    
    makePlaintextTable = function(sourceDir = "tsv", targetDir = "tsv", verbose = TRUE){
      if (verbose) message("... reading in basetable")
      basetable <- data.table::fread(file.path(self$dir, sourceDir, "basetable.tsv"))
      message("... extracting table with pain text and ids")
      texttable <- basetable[, c("id", "text"), with = TRUE]
      data.table::fwrite(
        texttable, file = file.path(self$dir, targetDir, "texttable.tsv"),
        showProgress = interactive()
        )
      invisible(texttable)
    },

    
    treetagger = function(sourceDir = "tok", targetDir = "vrt", lang = "de", ...){
      dirApply(
        f = treetagger,
        sourceDir = file.path(self$dir, sourceDir),
        targetDir=file.path(self$dir, targetDir),
        param = list(lang = lang),
        ...
      )
    },
    
    addTreetaggerLemmatization = function(sourceDir = "tsv", targetDir = "tsv", lang = "de", verbose = TRUE){
      if (verbose) message("... reading in tokenstream.tsv")
      tokenstreamDT <-  data.table::fread(file.path(self$dir, sourceDir, "tokenstream.tsv"), showProgress = interactive())
      
      # to make processing robus
      tokenstreamDT <- tokenstreamDT[!is.na(tokenstreamDT[["word"]])] # NAs cause problems!
      tokenstreamDT <- tokenstreamDT[which(tokenstreamDT[["word"]] != "")] # blank words too
      setkeyv(tokenstreamDT, cols = "id") # ndjson2tsv may have mixed up the order
      setorderv(tokenstreamDT, cols = "id")
      
      if (verbose) message("... writing column with tokens to temporary file")
      tmpdir <- tempdir()
      data.table::fwrite(
        tokenstreamDT[, "word", with = TRUE],
        file = file.path(tmpdir, "tokenstream.tok"),
        col.names = FALSE,
        quote = FALSE, showProgress = interactive()
      )
      
      if (verbose) message("... run treetagger")
      treetagger(sourceDir = tmpdir, targetDir = tmpdir, filename = "tokenstream.tok", param = list(lang = lang, tokenize = FALSE))
      
      if (verbose) message("... read in treetagger output and supplement tokenstream data.table")
      treetaggerOutput <- data.table::fread(
        file.path(tmpdir, "tokenstream.vrt"), sep = "\t", col.names = c("word", "pos", "lemma"),
        header = FALSE, showProgress = interactive()
      )
      tokenstreamDT[, lemma := treetaggerOutput[["lemma"]]]
      tokenstreamDT[, lemma := gsub("^<unknown>$", "#unknown#", tokenstreamDT[["lemma"]])]
      
      if (verbose) message("... write result back to tokenstream.tsv")
      data.table::fwrite(
        tokenstreamDT, file = file.path(self$dir, targetDir, "tokenstream.tsv"),
        sep = "\t", showProgress = interactive()
        )
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
    
    consolidate = function(sourceDir, targetDir, consolidation, element, attribute, ...){
      .consolidate <- function(filename, sourceDir, targetDir, verbose, param){
        startTime <- Sys.time()
        consolidation <- param[["consolidation"]]
        doc <- xmlTreeParse(file.path(sourceDir, filename), useInternalNodes=TRUE)
        nodes <- getNodeSet(doc, paste("//", param[["element"]]))
        for (i in 1:length(nodes)){
          attrValuesOld <- xmlAttrs(nodes[[i]])
          if (attrValuesOld[param[["attribute"]]] %in% names(consolidation)){
            attrValuesOld[param[["attribute"]]] <- consolidation[[ attrValuesOld[param[["attribute"]]] ]]
            xmlAttrs(nodes[[i]]) <- attrValuesOld
          }
        }
        xmlOut <- saveXML(doc, prefix='<?xml version="1.0" encoding="UTF-8"?>', indent=TRUE, encoding="UTF-8")
        if (is.null(targetDir)){
          return(xmlOut)
        } else {
          cat(xmlOut, file=file.path(targetDir, filename))
          return(as.character(Sys.time() - startTime))
        }
      }
      dirApply(
        f=.consolidate,
        sourceDir=file.path(self$dir, sourceDir),
        targetDir=file.path(self$dir, targetDir),
        param=list(consolidation=consolidation, element=element, attribute=attribute),
        ...
      )
    },
    
    fix = function(sourceDir, targetDir, encoding = "UTF-8", replacements = list(), ...){
      checkDirs(x, sourceDir, targetDir)
      dirApply(
        f = .repairVrtFile,
        sourceDir = file.path(x$dir, sourceDir),
        targetDir = file.path(x$dir, targetDir),
        param = list(encoding = encoding, replacements = replacements),
        ...
      )
    },
    
    getAttributeValues = function(sourceDir, pattern, element, attrs, unique = TRUE, mc = FALSE, progress = TRUE){
      getAttributeValues(
        .Object = file.path(self$dir, sourceDir), pattern = pattern,
        element = element, attrs = attrs, unique = unique, mc = mc, progress = progress
      )
    },
    
    tokenize = function(sourceDir, targetDir, with = "stanfordNLP", lang = "de", ...){
      dirApply(
        f = .tokenizeWorker, sourceDir=file.path(self$dir, sourceDir),
        targetDir = file.path(self$dir, targetDir),
        param = list(with = with, lang = lang), ...       
      )
    },
    
    tokenizeSentences = function(sourceDir = "xml",targetDir="xmlAnno", targetElement = "p", para = FALSE, ...){
      tokenizeSentences(
        .Object = file.path(sel$dir, sourceDir), targetDir=file.path(self$dir, targetDir),
        targetElement=targetElement, para=para, ...
      )
      return(NULL)
    },
    
    sAttributeList = function(sourceDir, sample = 100, ...){
      sAttributeList(file.path(self$dir, sourceDir), sample = sample, ...)
    },
    
    getNestedElements = function(sourceDir, corpus, element, max.embedding = NULL){
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
            file.path(self$dir, sourceDir),
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
    }
  )
)
