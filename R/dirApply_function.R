#' Apply a function over files in directory.
#' 
#' The function (f) will be applied to the files in a directory (in a lapply/sapply/vapply-style).
#' 
#' Function f passes in a function that will be applied to the individual files
#' in the source directory (sourceDir). The function needs to conform to a
#' standardized format, parameters required are "filename" (character),
#' "sourceDir" (character), "targetDir" (character) and "param" (list).
#' 
#' The function f can be applied in a multicore mode. If progress is \code{FALSE}, then
#' \code{mclapply} will be used. If progress is \code{TRUE}, \code{pblapply} from
#' the package \code{pbapply} will be used.
#' 
#' 
#' @param f function that will be applied to all files in sourceDir 
#' @param sourceDir source directory with files to be processed
#' @param targetDir target directory where the processed files will be saved
#' @param pattern pattern (can be a regex) that will be used by \code{list.files} as 'pattern'-argument
#' @param mc logical, whether to use multicore/parallel processing, if numeric, the number of cores to use
#' @param progress logical, whether to show progress bar
#' @param verbose logical, whether to be verbose
#' @param sample defults to FALSE, if a numeric, a number of random files to process
#' @param filenames character vector with filenames to be used, a subset of files in \code{sourceDir}
#' @param continue work only on those files not yet present in target dir
#' @param failsafe whether to be robust and catch errors using \code{try}, available only without parallelisation
#' @param param a list with parameters to be passed to the function f
#' @export dirApply
#' @rdname dirApply
#' @name dirApply
#' @import parallel
#' @importFrom tools file_path_sans_ext
#' @importFrom pbapply pblapply
#' @importFrom parallel detectCores
dirApply <- function(
  f, sourceDir, targetDir = NULL,
  pattern = NULL, mc = FALSE, progress = TRUE, verbose = FALSE,
  sample = FALSE, filenames = NULL, continue = FALSE, failsafe = FALSE,
  param = list()
  ){
  
  if (is.null(filenames)) filenames <- list.files(sourceDir, pattern)
  if (length(filenames) == 0) stop("no files in sourceDir")
  
  if (continue){
    if (verbose) message ("... getting files not yet present in targetDir")
    filesTargetDir <- list.files(targetDir)
    filesToGo <- filenames[!file_path_sans_ext(filenames) %in% file_path_sans_ext(filesTargetDir)]
    if (verbose){
      message("files in sourceDir -> ", length(filenames))
      message("files in targetDir -> ", length(filesTargetDir))
      message("skipping files -> ", length(filenames) - length(filesToGo))
    }
    filenames <- filesToGo
  }
  
  if (sample != FALSE) filenames <- sample(filenames, size = sample)
  
  if (mc == FALSE) {
    
    if (progress) verbose <- FALSE
    startTime <- Sys.time()
    retval <- lapply(
      setNames(1:length(filenames), filenames),
      function(i){
        if (verbose) message("... processing ", filenames[i])
        if (progress) .progressBar(i, length(filenames), showShare = TRUE, startTime = startTime)
        f <- f(
          filename = filenames[i], sourceDir = sourceDir, targetDir = targetDir,
          verbose = verbose, param = c(fileNo = i, filesTotal = length(filenames), param)
        )
        if (failsafe) f else try(f)
      }
    )
    if (unique(lapply(retval, function(x) class(x)[1])) == "difftime") class(retval) <- "timePerFile"
    return(retval)
    
  } else if (mc == TRUE || is.numeric(mc)){
    if (is.numeric(mc) == TRUE){
      noCores <- mc
      if (verbose) message("... using ", noCores, " cores")
    } else {
      noCores <- detectCores() - 1
      if (verbose) message("... number of cores not provided explicitly, using ", noCores, " cores")
    }
    if (progress == FALSE){
      retval <- mclapply(
        setNames(1:length(filenames), filenames),
        function(i) f(
          filenames[i], sourceDir = sourceDir, targetDir = targetDir,
          verbose = verbose, param = c(fileNo=i, filesTotal = length(filenames), param)
          ),
        mc.cores = noCores
        )  
    } else if (progress == TRUE){
      retval <- pblapply(
        setNames(1:length(filenames), filenames),
        function(i) f(
          filenames[i], sourceDir = sourceDir, targetDir = targetDir,
          verbose = verbose, param = c(fileNo=i, filesTotal = length(filenames), param)
        ),
        cl = noCores
      )
    }
    if (unique(lapply(retval, function(x) class(x)[1])) == "difftime") class(retval) <- "timePerFile"
    return(retval)
  }
}
