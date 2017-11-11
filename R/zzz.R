#' @importFrom utils packageVersion
.onAttach <- function(lib, pkg){
  packageStartupMessage(sprintf("ctk %s", packageVersion("ctk")))
  options(
    "ctk.stanfordDir" = Sys.getenv("CORENLP_PATH"),
    "ctk.propertiesFile" = system.file(package = "ctk", "propertiesFiles", "StanfordCoreNLP-german.properties"),
    "ctk.treetaggerDir" = Sys.getenv("TREETAGGER_PATH")
  )
  packageStartupMessage("... checking for TreeTagger ... ", appendLF = FALSE)
  treetaggerCheck <- system(file.path(getOption("ctk.treetaggerDir"), "bin", "tree-tagger"), ignore.stderr =  TRUE)
  if (treetaggerCheck == 1) packageStartupMessage("OK") else packageStartupMessage("FAIL")
  if (getOption("ctk.stanfordDir") != ""){
    packageStartupMessage("... checking for Stanford CoreNLP ... ", appendLF = FALSE)
    corenlpCmd <-  c(
      file.path(getOption("ctk.stanfordDir"), "corenlp.sh"),
      "-annotators tokenize",
      "-file", file.path(getOption("ctk.stanfordDir"), "input.txt"),
      "-outputDirectory", tempdir() # write output to some temporary directory
    )
    coreNlpCheck <- system(paste(corenlpCmd, collapse = " "), ignore.stdout = TRUE, ignore.stderr = TRUE)
    if (coreNlpCheck == 0) packageStartupMessage("OK") else packageStartupMessage("FAIL")
  }
  
}
