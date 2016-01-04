.formatDifftime <- function(diffTime){
  mins <- as.numeric(diffTime)
  secs <- mins - trunc(mins)
  hours <- floor(mins / 60)
  paste(c(
    sprintf("%02d", hours),  
    sprintf("%02d", floor(mins - hours * 60)),
    sprintf("%02d", floor(secs * 60))
    ), collapse=":" 
  )
}

.progressBar <- function(i, total, showShare=TRUE, startTime=NULL) {
  # no <- floor(barLength * (i/total))
  barLength <- 80
  addCharsToDelete <- 0
  if (showShare == TRUE){
    noDigitsMax <- nchar(as.character(total))
    addCharsToDelete <- addCharsToDelete + 2 * noDigitsMax + 12
    barLength <- barLength - 22 - 2 * noDigitsMax
    noFilled <- trunc((i/total) * barLength)
    noBlank <- barLength - noFilled
    if (noBlank < 0) noBlank <- 0
    shareDisplay <- paste(
      ifelse(i == total, "   ", "    "),
      "[",
      sprintf("%02d", trunc(i/total * 100)),
      "% | ",
      sprintf(paste("%0", as.character(noDigitsMax), "d", sep=""), i),
      "/",
      as.character(total),
      "]",
      sep=""
      )
  } else {
    shareDisplay <- ""
  }
  if (!is.null(startTime)){
    if (i > 0){
      currentTime <- Sys.time() 
      #timePassed <-  currentTime - startTime
      timePassed <- difftime(currentTime, startTime, units="mins")
      timePerItem <- timePassed / i
      estimatedProcessingTime <- timePerItem * total
      timeToGo <- abs(as.numeric(estimatedProcessingTime) - as.numeric(timePassed))
      expectedTimeRaw <- startTime + estimatedProcessingTime
      expectedTimeFormatted <- strftime(expectedTimeRaw, format="%H:%M:%S")
      currentTimeFormatted <- strftime(currentTime, format="%H:%M:%S")
      timeDisplay <- paste(c(
          paste("current: ", currentTimeFormatted, sep=""),
          paste("expected: ", expectedTimeFormatted, sep=""),
          paste("passed: ", .formatDifftime(timePassed), sep=""),
          paste("remaining: ", .formatDifftime(timeToGo), sep="")
        ),
        collapse=" | ", sep=""
      )
      timeDisplay <- paste("[", timeDisplay, "]", sep="")
      addCharsToDelete <- addCharsToDelete + nchar(timeDisplay)
    } else {
      timeDisplay <- ""
    }
    
  } else {
    timeDisplay <- ""
  }
  # if (i > 1) cat(paste(rep("\b", times=barLength+11+addCharsToDelete), collapse=""))
  if (i > 1) cat(paste(rep("\b", times=171), collapse=""))
  cat(paste(
    "0% [",
    paste(rep("=", times=noFilled), collapse=""),
    paste(rep(" ", times=noBlank), collapse=""), 
    "] 100%",
    shareDisplay,
    "\n",
    timeDisplay,
    sep="")
    )
  if (i == total) cat("\n")
}

.mv <- function(object, from, to, files){
  files <- list.files(from)
  files <- files[grep(regex, files)]
  for (file in files){
    file.copy(from=file.path(from, file), to=to)
  }
}

#' @param sourceDir the source directory
#' @param f function to be passed in
#' @param mc logical, whether to use multicore
#' @param verbose logical, whether to be verbose
#' @param progress logical, whether to show progress bar
#' @param ... parameters that will be passed into worker function
#' @noRd
.iterateFunctionFiles <- function(sourceDir, f, pattern="xml", mc=FALSE, verbose=FALSE, progress=TRUE, ...){
  files <- list.files(sourceDir, pattern)
  if (mc == FALSE) {
    lapply(c(1:length(files)), function(x){
      f(files[x], sourceDir=sourceDir, verbose=verbose, ...)
      if (verbose == FALSE) .progressBar(x, length(files))
    })
  } else {
    mclapply(files, function(file) f(file, sourceDir=sourceDir, verbose=verbose, ...))
  }
}



.subdirs <- function(.Object){
  subdirs <- list.dirs(.Object@projectDir, full.names=FALSE)
  subdirs <- subdirs[which(subdirs != "")]
  .Object@subdirs <- setNames(as.character(rep(NA, times=length(subdirs))), subdirs)  
  return(.Object)
}

.setPaths <- function(.Object){
  if ("CORPUS_REGISTRY" %in% names(Sys.getenv())) .Object@cwbRegistry <- Sys.getenv("CORPUS_REGISTRY")
  if ("PATH_SAXON" %in% names(Sys.getenv())) .Object@saxonPath <- Sys.getenv("PATH_SAXON")
  if ("PATH_TREETAGGER" %in% names(Sys.getenv())) .Object@treetaggerPath <- Sys.getenv("PATH_TREETAGGER")  
  return(.Object)
}


#' remove leading and trailing whitespace of a character vector
#' 
#' @export removeWhitespace
removeWhitespace <- function(x, rmBlankLines=TRUE){
  x <- gsub("^\\s*(.*?)\\s*$", "\\1", x)
  # x <- x[-grep("^\\s*$", x)]
  if (rmBlankLines){
    xList <- as.list(x)
    for (i in c(length(x):1)) if (grepl("^\\s*$", x)) xList[[i]] <- NULL
    x <- unlist(xList)    
  }
}

#' remove empty lines
#' 
#' @param x
#' @export removeEmptyLines
#' @rdname removeEmptyLines
#' @name removeEmptyLines
removeEmptyLines <- function(x){
  emptyLines <- grep("^\\s*$", x)
  if (length(emptyLines) > 0 ){
    for (i in rev(emptyLines)) x <- x[-i]
  } 
  x
}

#' @importFrom stringi stri_match_all_regex
#' @export normalizeGermanDate 
#' @rdname normalizeGermanDate
#' @name normalizeGermanDate
normalizeGermanDate <- function(dateRaw){
  monthToNumber <- c(
    Januar=1, Februar=2, März=3, April=4, Mai=5, Juni=6, Juli=7, August=8, September=9, Oktober=10, November=11, Dezember=12
  )
  dateRegex <- paste(
    "^\\s*(\\d+)\\.\\s+(",
    paste(names(monthToNumber), collapse="|"),
    ")\\s+(\\d{4})\\s*$", sep=""
  )
  regexMatch <- stri_match_all_regex(dateRaw, pattern=dateRegex)[[1]][1,]
  paste(
    regexMatch[4],
    sprintf("%02d", as.numeric(monthToNumber[regexMatch[3]])),
    sprintf("%02d", as.numeric(regexMatch[2])),
    sep="-"
  )
}

#' @export regexDate
regexDate <- list(
  de="\\d+\\.\\s+(Januar|Februar|März|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember)\\s+\\d+"
)


.as.human <- function(x){
  data.frame(
    i_index=x$i,
    i_explicit=x$dimnames[[1]][x$i],
    j_index=x$j,
    j_explicit=x$dimnames[[1]][x$j],
    value=x$v
  )
}

