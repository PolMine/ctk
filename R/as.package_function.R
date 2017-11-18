#' Put CWB indexed corpus into package.
#' 
#' @param corpus a corpus detailed in CORPUS_REGISTRY dir
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
    "R", "man", "inst",
    c("inst", "extdata"),
    c("inst", "extdata", "cwb"),
    c("inst", "extdata", "cwb", "registry"),
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
    Version = if (is.null(version)) "0.1.0" else version,
    Date = if (is.null(date)) as.character(Sys.Date()) else date,
    Author = author,
    Maintainer = if (is.null(maintainer)) author else maintainer,
    Depends = "", LazyData = "yes",
    Description = if (is.null(description)) pkg else description,
    License = if (is.null(license)) "0.1.0" else license
  )
  description_char <- sapply(
    names(description_list),
    function(x) paste(x, description_list[[x]], sep = ": ")
  )
  cat(description_char, file = file.path(targetDir, "DESCRIPTION"), sep = "\n")
  
  if (verbose) message("... copy binary corpus files")
  filesToCopy <- list.files(polmineR::RegistryFile$new(corpus)$getHome(), full.names = TRUE)
  dummy <- lapply(
    filesToCopy,
    function(x) {
      if (verbose == TRUE) message("... moving ", x)
      file.copy(from = x, to = file.path(targetDir, "inst", "extdata", "cwb", "indexed_corpora", tolower(corpus)))
    }
  )
}
