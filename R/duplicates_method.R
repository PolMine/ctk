#' .consolidateDate <- function(rawDate){
#'   consolidatedDate <- gsub("Juillet", "08", rawDate)
#'   if (!grepl("-", consolidatedDate)) consolidatedDate <- gsub("^(\\d{4})(\\d{2})(\\d+)$", "\\1-\\2-\\3", consolidatedDate)
#'   consolidatedDate <- as.character(strptime(as.POSIXct(consolidatedDate), format="%Y-%m-%d"))
#'   consolidatedDate
#' }
#' 
#' .getDate <- function(filename, sourceDir, targetDir, verbose=NULL, param){
#'   xmlDoc <- xmlParse(file.path(sourceDir, filename))
#'   dateOfDoc <- unname(xmlAttrs(getNodeSet(xmlDoc, path=param$dateElement)[[1]])[param$dateAttribute])
#'   .consolidateDate(dateOfDoc)
#' }
#' 
#' .getLength <- function(filename, sourceDir, targetDir=NULL, verbose=FALSE, param=list()){
#'   unname(nchar(getChildrenStrings(xmlParse(file.path(sourceDir, filename)))))
#' }
#' 
#' 
#' setGeneric("getPotentialDuplicates", function(.Object, ...) standardGeneric("getPotentialDuplicates"))
#' 
#' #' get potential duplicates
#' #' 
#' #' @examples 
#' #' \dontrun{
#' #' xmlDir <- "/Users/blaette/Lab/repos/keywords/data/figaro/xml"
#' #' docsToCompareMatrix <- getPotentialDuplicates(xmlDir, reduce=TRUE, progress=TRUE, verbose=TRUE, mc=TRUE)
#' #' }
#' setMethod("getPotentialDuplicates", "character", function(.Object, dateElement="/text", dateAttribute="day", reduce=TRUE, verbose=FALSE, progress=TRUE, mc=FALSE){
#'   filenames <- list.files(.Object)
#'   if (verbose == TRUE) message("... getting dates from XML files")
#'   dates <- unlist(
#'     dirApply(
#'       f=.getDate, sourceDir=.Object, targetDir=NULL,
#'       progress=progress, verbose=verbose, mc=mc,
#'       param=list(dateElement=dateElement, dateAttribute=dateAttribute)
#'     ))
#'   if (mc != FALSE) dates <- dates[filenames] # just to be sure that the order is correct
#'   if (verbose == TRUE) message("... getting files to be compared")
#'   filenameIndexSplittedByDate <- split(c(1:length(filenames)), f=dates)
#'   .getDocsToCompare <- function(i){
#'     dateOfDoc <- as.POSIXct(dates[i])
#'     datesToGet <- c(
#'       as.character(strptime(dateOfDoc - 60, format="%Y-%m-%d")),
#'       as.character(strptime(dateOfDoc, format="%Y-%m-%d")),
#'       as.character(strptime(dateOfDoc + 86401, format="%Y-%m-%d"))
#'     )
#'     unlist(lapply(datesToGet, function(x) filenameIndexSplittedByDate[[x]]))
#'   }
#'   if (mc == FALSE){
#'     docsToCompare <- lapply(c(1:length(filenames)), .getDocsToCompare)
#'   } else {
#'     docsToCompare <- mclapply(c(1:length(filenames)), .getDocsToCompare)
#'   }
#'   docsToCompareMatrix <- simple_triplet_matrix(
#'     i=unlist(docsToCompare),
#'     j=unlist(lapply(c(1:length(docsToCompare)), function(i) rep(i, times=length(docsToCompare[[i]])))),
#'     v=rep(NA, times=length(unlist(docsToCompare))),
#'     ncol=length(filenames),
#'     nrow=length(filenames),
#'     dimnames=list(rows=filenames, columns=filenames)
#'   )
#'   if (reduce == TRUE){
#'     keepOrDrop <- sapply(
#'       c(1:length(docsToCompareMatrix$i)),
#'       function(i) ifelse(docsToCompareMatrix$i[i] < docsToCompareMatrix$j[i], TRUE, FALSE)
#'     )
#'     for (x in c("i", "j", "v")) docsToCompareMatrix[[x]] <- docsToCompareMatrix[[x]][keepOrDrop]
#'   }
#'   return(docsToCompareMatrix)
#' })
#' 
#' setGeneric("getDuplicates", function(.Object, ...) standardGeneric("getDuplicates"))
#' 
#' #' get duplicates 
#' #' 
#' #' @examples 
#' #' \dontrun{
#' #' xmlDir <- "/Users/blaette/Lab/repos/keywords/data/figaro/xml"
#' #' charCount <- ctk::characterCount(xmlDir, toLower=TRUE, progress=TRUE, verbose=TRUE, mc=3)
#' #' ngramMatrix <- ctk::getNgrams(xmlDir, charCount=noChars, nChar=10, progress=TRUE, mc=3, verbose=TRUE)
#' #' ngramMatrixWeighed <- polmineR::weigh(ngramMatrix, method="tfidf")
#' #' docsToCompareMatrix <- getPotentialDuplicates(xmlDir, reduce=TRUE, progress=TRUE, verbose=TRUE, mc=3)
#' #' similarityMatrix <- polmineR::similarity(x=ngramMatrixWeighed, select=docsToCompareMatrix, verbose=TRUE, progress=TRUE, mc=3)
#' #' duplicates <- getDuplicates(xmlDir, similarityMatrix=similarityMatrix, progress=TRUE, mc=3, verbose=TRUE)
#' #' annotatedXML <- annotateDuplicates(
#' #'   .Object=xmlDir, targetDir=NULL,
#' #'   duplicates=duplicates, verbose=TRUE, mc=FALSE, progress=TRUE
#' #'   )
#' #' }
#' setMethod("getDuplicates", "character", function(.Object, similarityMatrix, threshold=0.9, ...){
#'   sourceDir <- .Object
#'   if (verbose == TRUE) message("... applying threshold")
#'   if (mc == FALSE) mc <- 1
#'   indexDuplicates <- which(similarityMatrix$v >= threshold)
#'   for (x in c("i", "j", "v")) similarityMatrix[[x]] <- similarityMatrix[[x]][indexDuplicates]
#'   if (verbose == TRUE) message("... getting date and length from XML files")
#'   indexRelevantFiles <- unique(c(similarityMatrix$i, similarityMatrix$j))
#'   filenamesRelevantFiles <- similarityMatrix[["dimnames"]][[1]][indexRelevantFiles]
#'   if (progress == FALSE){
#'     docInfoList <- mclapply(
#'       setNames(filenamesRelevantFiles, filenamesRelevantFiles),
#'       function(x){
#'         list(date=.getDate(x, sourceDir), length=.getLength(x, sourceDir))
#'       }, mc.cores=mc
#'     )
#'   } else if (progress == TRUE){
#'     .getDocInfoList <- function(filename, sourceDir, targetDir, verbose, param){
#'       list(date=.getDate(filename, sourceDir), length=.getLength(filename, sourceDir))
#'     }
#'     docInfoList <- dirApply(f=.getDocInfoList, sourceDir=.Object, targetDir=NULL, ...)
#'   }
#'   duplicateList <- lapply(
#'     c(1:length(similarityMatrix$i)),
#'     function(i){
#'       iFilename <- similarityMatrix$dimnames[[1]][similarityMatrix$i[i]]
#'       jFilename <- similarityMatrix$dimnames[[1]] [similarityMatrix$j[i]]
#'       iDate <- as.POSIXct(docInfoList[[iFilename]][["date"]])
#'       iLength <- docInfoList[[iFilename]][["length"]]
#'       jDate <- as.POSIXct(docInfoList[[jFilename]][["date"]])
#'       jLength <- docInfoList[[jFilename]][["length"]]
#'       value <- similarityMatrix$v[i]
#'       if(iDate == jDate){
#'         if (iLength >= jLength){
#'           return(c(filename=iFilename, duplicateFilename = jFilename, value=value))
#'         } else {
#'           return(c(filename=jFilename, duplicateFilename = iFilename, value=value))
#'         }
#'       } else if (iDate < jDate){
#'         return(c(filename=iFilename, duplicateFilename=jFilename, value=value))
#'       } else if (iDate > jDate){
#'         return(c(filename=jFilename, duplicateFilename=iFilename, value=value))
#'       }
#'     })
#'   duplicateUnique <- unique(sapply(duplicateList, function(x) paste(x, collapse="\t")))
#'   duplicateFinal <- as.data.frame(
#'     do.call(rbind, strsplit(duplicateUnique, "\t")),
#'     stringsAsFactor=FALSE
#'   )
#'   duplicateFinal[,1] <- as.character(as.vector(duplicateFinal[,1]))
#'   duplicateFinal[,2] <- as.character(as.vector(duplicateFinal[,2]))
#'   duplicateFinal[,3] <- as.numeric(as.vector(duplicateFinal[,3]))
#'   colnames(duplicateFinal) <- c("filename", "duplicateFilename", "value")
#'   duplicateFinal
#' })
#' 
#' setGeneric("annotateDuplicates", function(.Object, ...) standardGeneric("annotateDuplicates"))
#' 
#' setMethod("annotateDuplicates", "character", function(.Object, targetDir, duplicates = NULL, verbose = TRUE, mc = FALSE, progress = TRUE){
#'   if (is.null(duplicates)){
#'     sourceDir <- .Object
#'     charCount <- characterCount(sourceDir, toLower=TRUE, progress=progress, verbose=verbose, mc=mc)
#'     ngramMatrix <- getNgrams(sourceDir, charCount=charCount, nChar=10, progress=progress, mc=mc, verbose=verbose)
#'     docsToCompareMatrix <- getPotentialDuplicates(sourceDir, reduce=TRUE, progress=progress, verbose=verbose, mc=mc)
#'     # weighing ngramMatrix missing?!
#'     similarityMatrix <- polmineR.misc::similarity(x=ngramMatrix, select=docsToCompareMatrix, weighting="tfidf", verbose=verbose, progress=progress, mc=mc)
#'     duplicates <- getDuplicates(sourceDir, similarityMatrix=similarityMatrix, progress=progress, mc=mc, verbose=verbose)
#'   }
#'   .annotateDuplicate <- function(filename, sourceDir, targetDir, verbose, param){
#'     xmlDoc <- xmlParse(file.path(sourceDir, filename))
#'     rootNode <- getNodeSet(xmlDoc, path="/text")[[1]]
#'     xmlAttrs(rootNode) <- param$newAttributes[[filename]]
#'     xmlOutput <- saveXML(
#'       xmlDoc, prefix='<?xml version="1.0" encoding="UTF-8"?>\n',
#'       file=NULL, indent=TRUE, encoding="UTF-8"
#'     )
#'     if (is.null(targetDir)){
#'       retval <- xmlOutput
#'     } else {
#'       cat(xmlOutput, file=file.path(targetDir, filename), sep="\n")  
#'       retval <- Sys.time()
#'     }
#'     retval
#'   }
#'   filenames <- list.files(sourceDir)
#'   newAttributes <- lapply(
#'     setNames(filenames, filenames),
#'     function(x){
#'       retval <- c(isDuplicate="FALSE", isDuplicateOf="")
#'       if (x %in% duplicates[["duplicateFilename"]]){
#'         retval <- c(
#'           isDuplicate="TRUE",
#'           isDuplicateOf = paste(duplicates[which(duplicates[["duplicateFilename"]] == x),][,"filename"], collapse="|")
#'         )
#'       }
#'       retval
#'     }
#'   )
#'   dirApply(
#'     f=.annotateDuplicate, sourceDir=.Object, targetDir=targetDir,
#'     verbose=verbose, mc=mc, progress=progress,
#'     param=list(newAttributes=newAttributes)
#'   )
#' })
