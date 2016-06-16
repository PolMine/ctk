.readRegistry <- function(corpus){
  scan(
    file=file.path(Sys.getenv("CORPUS_REGISTRY"), tolower(corpus)),
    sep="\n",
    what="character",
    quiet=TRUE, blank.lines.skip=FALSE
  )
}

.parseRegistry <- function(corpus){
  registry <- readRegistry(corpus)
  registryList <- lapply(
    setNames(c("NAME", "ID", "HOME", "INFO"), c("NAME", "ID", "HOME", "INFO")),
    function(query){
      gsub(paste("^", query, "\\s+(.*?)$", sep=""), "\\1", grep(paste("^", query, sep=""), registry, value=T), perl=T)
    })
  # get pAttributes
  registryList[["pAttributes"]] <- gsub("^ATTRIBUTE\\s+(.*?)$", "\\1", grep("^ATTRIBUTE", registry, value=T))
  # get language
  registryList[["language"]] <- gsub("^.*language\\s=\\s\"(.*?)\".*$", "\\1", grep("language =", registry, value=T))
  # getEncoding
  encodingLine <- registry[grep('charset\\s*=\\s*"', registry)]
  encoding <- sub('^.*charset\\s*=\\s*"(.+?)".*$', "\\1", encodingLine)
  encoding <- toupper(encoding)
  if (!encoding %in% iconvlist()){
    warning('Please check encoding in the registry file (charset="..." provides unknown encoding) or provide encoding explicitly')
  }
  registryList[["encoding"]] <- tolower(encoding)
  return(registryList)
}


#' create R data package with corpus
#' 
#' @param corpus a corpus detailes in CORPUS_REGISTRY dir
#' @param targetDir the target directory
#' @param author the author of the package
#' @param pkg package name
#' @param version the version number of the corpus
#' @param date date of creation
#' @param maintainer maintainer, R package style
#' @param description short description of the data package
#' @param licence the license
#' @param verbose whether to be verbose
#' @export as.package
#' @rdname as.package
#' @name as.package
as.package <- function(
  corpus, targetDir, author, pkg=NULL, version=NULL,
  date=NULL, maintainer="Andreas Blaette <andreas.blaette@uni-due.de>",
  description=NULL, license="NONE", verbose=TRUE
  ){
  if (verbose == TRUE) message("... creating directories")
  stopifnot (file.exists(targetDir) == TRUE)
  dirs <- list(
    #    c("R"), c("man"),
    c("inst"), c("inst", "extdata"),
    c("inst", "extdata", "cwb"),
#    c("inst", "extdata", "cwb", "registry"),
    c("inst", "extdata", "cwb", "indexed_corpora"),
    c("inst", "extdata", "cwb", "indexed_corpora", tolower(corpus))
  )
  for (dir in dirs) {
    newDir <- file.path(targetDir, paste(dir, collapse="/"))
    if (file.exists(newDir) == FALSE) dir.create(newDir)
  }
  
  if (verbose == TRUE) message("... creating DESCRIPTION file")
  if (is.null(pkg)) pkg <- strsplit(targetDir, "/")[[1]][length(strsplit(targetDir, "/")[[1]])]
  descriptionList <- list(
    Package=pkg, Type="Package", Title=pkg,
    Version=ifelse(is.null(version), "0.1.0", version),
    Date=ifelse(is.null(date), as.character(Sys.Date()), date),
    Author=author,
    Maintainer=ifelse(is.null(maintainer), author, maintainer),
    Depends="", LazyData="yes",
    Description=ifelse(is.null(description), pkg, description),
    License=ifelse(is.null(license), "0.1.0", license)
  )
  descriptionChar <- sapply(
    names(descriptionList),
    function(x) paste(x, descriptionList[[x]], sep=": ")
  )
  cat(descriptionChar, file=file.path(targetDir, "DESCRIPTION"), sep="\n")
  
  if (verbose == TRUE) message("... creating configure file")
  registryParsed <- .parseRegistry(corpus)
  registry <- .readRegistry(corpus)
#  cat(registry, file=file.path(targetDir, "inst", "extdata", "cwb", "registry", tolower(corpus)), sep="\n")
  registryHead <- paste(
    registry[1:grep("# path to binary data files", registry)],
    collapse="LINEBREAK"
    )
  registryTail <- paste(
    registry[grep("# corpus properties", registry):length(registry)],
    collapse="LINEBREAK"
    )
  configureFile <- scan(
    file=system.file("sh", "configure", package="ctk"),
    what="character", sep="\n", blank.lines.skip=FALSE, quiet=TRUE
    )
  configureFile <- gsub("CORPUS", tolower(corpus), configureFile)
  configureFile <- gsub("REGISTRY_HEAD", registryHead, configureFile)
  configureFile <- gsub("REGISTRY_TAIL", registryTail, configureFile)
  configureFile <- paste(configureFile, collapse="\n")
  cat(configureFile, file=file.path(targetDir, "configure"))
  sedShCmd <- paste("sh", system.file("sh", "sed.sh", package="ctk"), file.path(targetDir, "configure"))
  system(sedShCmd)
  system(paste("chmod", "0755", file.path(targetDir, "configure")))
  # system(paste("sed -e "" -i 's/LINEBREAK/\\\n/g'", file.path(targetDir, "configure"), sep=" "))
  
  
  if (verbose == TRUE) message("... copy binary corpus files")
  filesToCopy <- list.files(registryParsed$HOME, full.names=TRUE)
  dummy <- lapply(
    filesToCopy,
    function(x) {
      if (verbose == TRUE) message("... moving ", x)
      file.copy(from=x, to=file.path(targetDir, "inst", "extdata", "cwb", "indexed_corpora", tolower(corpus)))
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
    list.files(indexedCorpusDir, full.names = T),
    function(x) file.copy(from=x, to=file.path(indexedCorporaDir, registryFile))
    )
}
