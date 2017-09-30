#' create R data package with corpus
#' 
#' @param corpus a corpus detailes in CORPUS_REGISTRY dir
#' @param targetDir the target directory
#' @param author the author of the package
#' @param pkg package name (may not include special chars, no _)
#' @param version the version number of the corpus
#' @param date date of creation
#' @param maintainer maintainer, R package style
#' @param description short description of the data package
#' @param license the license
#' @param verbose whether to be verbose
#' @export as.package
#' @rdname as.package
#' @name as.package
#' @importFrom polmineR RegistryFile
as.package <- function(
  corpus, targetDir, author, pkg = NULL, version = NULL,
  date = NULL, maintainer = "Andreas Blaette <andreas.blaette@uni-due.de>",
  description = NULL, license = "NONE", verbose = TRUE
  ){
  if (verbose) message("... creating directories")
  stopifnot (file.exists(targetDir) == TRUE)
  dirs_to_create <- list(
    c("R"), c("man"),
    c("inst"), c("inst", "extdata"),
    c("inst", "extdata", "cwb"),
#    c("inst", "extdata", "cwb", "registry"),
    c("inst", "extdata", "cwb", "indexed_corpora"),
    c("inst", "extdata", "cwb", "indexed_corpora", tolower(corpus))
  )
  for (dir in dirs_to_create) {
    newDir <- file.path(targetDir, paste(dir, collapse = "/"))
    if (!file.exists(newDir)){
      if (verbose) message("... creating directory: ", newDir)
      dir.create(newDir)
    } 
  }
  
  if (verbose) message("... creating DESCRIPTION file")
  if (is.null(pkg)){
    pkg <- strsplit(targetDir, "/")[[1]][length(strsplit(targetDir, "/")[[1]])]
    if (verbose) message("... package name not provided, using extract from path: ", pkg)
  }
  description_list <- list(
    Package = pkg, Type = "Package", Title = pkg,
    Version = ifelse(is.null(version), "0.1.0", version),
    Date = ifelse(is.null(date), as.character(Sys.Date()), date),
    Author = author,
    Maintainer = ifelse(is.null(maintainer), author, maintainer),
    Depends="", LazyData="yes",
    Description = ifelse(is.null(description), pkg, description),
    License = ifelse(is.null(license), "0.1.0", license)
  )
  description_char <- sapply(
    names(description_list),
    function(x) paste(x, description_list[[x]], sep=": ")
  )
  cat(description_char, file = file.path(targetDir, "DESCRIPTION"), sep = "\n")
  
  if (verbose) message("... copy binary corpus files")
  filesToCopy <- list.files(polmineR::RegistryFile$new(corpus)$getHome(), full.names=TRUE)
  dummy <- lapply(
    filesToCopy,
    function(x) {
      if (verbose == TRUE) message("... moving ", x)
      file.copy(from = x, to = file.path(targetDir, "inst", "extdata", "cwb", "indexed_corpora", tolower(corpus)))
    }
  )
}

#' @rdname as.package
#' @export unpack
unpack <- function(pkg, verbose=TRUE){
  message("... get registry from package")
  pkgCwbDir <- file.path(system.file(package=pkg), "extdata", "cwb")
  registryFile <- list.files(file.path(pkgCwbDir, "registry"))
  registry <- scan(
    file=list.files(file.path(pkgCwbDir, "registry"), full.names=T),
    sep="\n", what="character", quiet=TRUE, blank.lines.skip=FALSE
  )
  
  if (registryFile %in% list.files(Sys.getenv("CORPUS_REGISTRY"))){
    input <- readline("corpus already exists - overwrite it (yes/no)?\n")
    stopifnot(input == "yes")
  }
  
  message("... adjust and save new registry")
  indexHomeLine <- grep("HOME", registry)
  indexInfoLine <- grep("INFO", registry)
  pathRegistryDirSplitted <- strsplit(Sys.getenv("CORPUS_REGISTRY"), "/")[[1]]
  cwbDir <- paste(pathRegistryDirSplitted[1:length(pathRegistryDirSplitted)-1], collapse="/")
  dirs <- list.files(cwbDir)
  indexedCorporaDir <- paste(cwbDir, grep("index", dirs, value=T), sep="/")
  registry[indexHomeLine] <- paste("HOME", file.path(indexedCorporaDir, registryFile))
  registry[indexInfoLine] <- paste("INFO", file.path(indexedCorporaDir, registryFile, ".info"))
  registryTargetFile <- file.path(Sys.getenv("CORPUS_REGISTRY"), registryFile)
  cat(registry, file=registryTargetFile, sep="\n")
  
  message("... copy binary corpus files from package")
  indexedCorpusDirPkg <- file.path(pkgCwbDir, "indexed_corpora", registryFile)
  lapply(
    list.files(indexedCorpusDirPkg, full.names = TRUE),
    function(x) file.copy(from=x, to=file.path(indexedCorporaDir, registryFile))
    )
}
