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


#' @name removeWhitespace
#' @title Remove whitespace.
#' @description Remove leading and trailing whitespace from a character vector.
#' @param x a character vector to process
#' @param rmBlankLines logical, whether to remove blank lines
#' @export removeWhitespace
removeWhitespace <- function(x, rmBlankLines=TRUE){
  x <- gsub("^\\s*(.*?)\\s*$", "\\1", x)
  if (rmBlankLines){
    xList <- as.list(x)
    for (i in length(x):1) if (grepl("^\\s*$", x)) xList[[i]] <- NULL
    x <- unlist(xList)    
  }
  x
}

#' remove empty lines
#' 
#' @param x object
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

#' @rdname normalizeGermanDate
monthsToNumeric <- list(
  "Januar" = "01", "Februar" = "02", "M\uE4rz" = "03",
  "April" = "04", "Mai" = "05", "Juni" = "06",
  "Juli" = "07", "August" = "08", "September" = "09",
  "Oktober" = "10", "November" = "11", "Dezember" = "12"
)


#' normalize german date 
#' 
#' @param dateRaw date to process
#' @importFrom stringi stri_match_all_regex
#' @export normalizeGermanDate 
#' @rdname normalizeGermanDate
#' @name normalizeGermanDate
normalizeGermanDate <- function(dateRaw){
  monthToNumber <- c(
    Januar = 1, Februar = 2, "M\uE4rz" = 3, April = 4,
    Mai = 5, Juni = 6, Juli = 7, August = 8,
    September = 9, Oktober = 10, November = 11, Dezember = 12
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
    sep = "-"
  )
}

#' @rdname normalizeGermanDate
regexDate <- list(
  de="\\d+\\.\\s+(Januar|Februar|M\uE4rz|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember)\\s+\\d+"
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

editRegistry <- function(
  corpus, name=NULL, id=NULL, home=NULL, info=NULL,
  charset=NULL, language=NULL, properties=NULL
  ){
  registryFilename <- file.path(Sys.getenv("CORPUS_REGISTRY"), tolower(corpus))
  registry <- scan(registryFilename, what="character", blank.lines.skip=FALSE, sep="\n", quiet=TRUE)
  if (!is.null(name)) registry[grep("^NAME", registry)] <- paste("NAME", name, sep=" ")
  if (!is.null(id)) registry[grep("^ID", registry)] <- paste("ID", id, sep=" ")
  if (!is.null(home)) registry[grep("^ID", registry)] <- paste("HOME", home, sep=" ")
  if (!is.null(info)) registry[grep("^ID", registry)] <- paste("INFO", info, sep=" ")
  if (!is.null(language)){
    i <- grep("##::\\s+language", registry)
    registry[i] <- gsub('".*?"', paste('"', language, '"', sep=""), registry[i])
  }
  if (!is.null(charset)){
    i <- grep("##::\\s+charset", registry)
    registry[i] <- gsub('".*?"', paste('"', charset, '"', sep=""), registry[i])
  }
  if (!is.null(properties)){
    propertyLines <- grep("^##::", registry)
    lastPropertyLine <- propertyLines[length(propertyLines)]
    newPropertyLines <- unlist(lapply(
      names(properties),
      function(x) paste(
        "##:: ", x, ' = "', paste(properties[[x]], collapse='|'), '"', sep=''
      )
      ))
    registry <- c(
      registry[1:lastPropertyLine],
      newPropertyLines,
      registry[(lastPropertyLine + 1):(length(registry))]
      )
  }
  cat(registry, file=registryFilename, sep="\n")
}
