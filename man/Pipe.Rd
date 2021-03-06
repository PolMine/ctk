% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Pipe_class.R
\docType{data}
\name{Pipe}
\alias{Pipe}
\title{Pipe for corpus preparation.}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
Pipe
}
\description{
The \code{Pipe} class offers a framework for corpus preparation
  and auxiliary tools. The methods of the class (wrappers for standard tools
  or helpers) use subdirectories of a pipe directory to take files and their
  content through the different stages of corpus preparation. To use Stanford
  CoreNLP, the class is extended by the \code{PipeCoreNLP} class.
}
\section{Fields}{

\describe{
\item{\code{dir}}{a pipe directory, different processing stages of the corpus will be kept in
subdirectories of this directory}

\item{\code{time}}{a data.frame with information that different processing stages have consumed}

\item{\code{threads}}{an integer, the number of cores to use}
}}

\section{Usage}{

For usage details see \bold{Methods, Arguments and Fiels} sections.
}

\section{Methods}{

\describe{
  \item{\code{$new(dir, threads = 1L)}}{Initialize new \code{Pipe} object.}
  \item{\code{$summary()}}{Return \code{data.frame} with number of files in
  the subdirectories of the pipe directory.}
  \item{\code{$preparePipeDir(subdirs = character(), delete = FALSE, verbose = 
  TRUE)}}{Create subdirectories provided by \code{subdirs} in the pipe 
  directory, and delete existing files, if \code{delete} is \code{TRUE}.}
  \item{\code{$getFiles(sourceDir, targetDir, ...)}}{Copy Files from 
  directories indicated by to the subdirectory of the pipe directory defined 
  by \code{targetDir}. See documentation for helper function \code{getFiles} 
  for options available through \code{...}.}
  \item{\code{$getMissingFiles(sourceDir, targetDir, ignore.extensions =
  TRUE)}}{ Identify files that are present in \code{sourceDir}, but not in 
  \code{targetDir}. If \code{ignore.extensions} is \code{TRUE}, file
  extensions are removed before comparing filenames.}
  \item{\code{$rsync()}}{Prepare rsync command that can be used to
  synchronize pipe directory with a remote storage.}
  \item{\code{mergeXMLFiles(sourceDir, targetDir, regex, rootElement,
  rootAttrs, mc=FALSE, verbose=TRUE, ...)}}{Merge files into single XML
  documents for faster processing during later stages of the pipe.}
  \item{\code{$validate(sourceDir, targetDir = NULL, dtd = NULL, 
  ...)}}{Validate that all XML files in \code{sourceDir} are valid XML files.
  if \code{dtd} is provided, check against a DTD.}
  \item{\code{$getAttributeValues(sourceDir, pattern, element, attrs, unique 
  = TRUE, mc = FALSE, progress = TRUE)}}{Get values of XML attributes defined
  by \code{attrs} for the element defined by \code{element}.}
  \item{\code{$consolidate(sourceDir, targetDir, consolidation, element, 
  attribute, ...)}}{Perform replacements for XML attributes as provided by 
  character vector \code{consolidation}. (Further documentation is needed!)}
  \item{\code{$xmlToDT(sourceDir = "xml", targetDir = "tsv", 
  metadata)}}{Extract text and metadata from XML documents, and write resulting
  'basetable' as tsv file to subdirectory specified by targetDir. The basetable
  is returned invisibly.}
  \item{\code{addTreetaggerLemmatization(sourceDir = "tsv", targetDir = 
  "tsv", lang = "de", verbose = TRUE)}}{The method will look for a file
  'tokenstream.tsv' in the subdirectory of the pipeDir specified by
  sourceDir. To use the treetagger, a temporary file is created (tokenstream
  only) and annotated. The result is read in again, added to the original
  table and saved to an updated file tokenstream.tsv in the targetDir. If
  sourceDir and targetDir are identical, the original file is overwritten.}
  \item{\code{$makeMetadataTable(sourceDir = "tsv", targetDir = "tsv",
  verbose = TRUE)}}{Dissect file basetable.tsv in sourceDir into 'texttable'
  and 'metadata' as more memory efficient ways for keeping the data. If
  targetDir is not NULL, the resulting tables will be stored as tsv files in
  the respective subdirectory of the pipe directory.}
  \item{\code{$makePlaintextTable(sourceDir = "tsv", targetDir = "tsv", 
  verbose = TRUE)}}{Dissect basetable into 'texttable' and 'metadata' as more
  memory efficient ways for keeping the data. If targetDir is not NULL, the
  resulting tables will be stored as tsv files in the respective subdirectory
  of the pipe directory.}
  \item{\code{$xslt(sourceDir, targetDir, xslFile, ...)}}{Perform XSL
  transformation.}
  \item{\code{$subset(sourceDir, targetDir, sample = NULL, files =
  NULL)}}{Generate a subset of files in the \code{sourceDir}, copying a
  choice of files to \code{targetDir}. If \code{files} is a character vector
  with filenames, it will be these files that are copied. If \code{sample} is
  provided (a number), a random sample is drawn.}
  \item{\code{$recode(sourceDir, targetDir, from = "UTF-8", to = 
  "ISO-8859-1", xml = FALSE, log = FALSE, ...)}}{Recode files in 
  \code{sourceDir}, writing results to \code{targetDir}. See documentation 
  for worker function \code{recode} for options that are available.}
  \item{\code{$replaceInvalidCharacters(sourceDir, targetDir, xml = FALSE,
  ...)}}{Replace characters that are known to cause problems.} 
  \item{\code{$findAndReplace(sourceDir, targetDir, replacements, encoding,
  ...)}}{Find matches for a regular expression and perform replacemet;
  \code{replacements} is a list of length 2 character vectors, which provide
  the regex and the replacement.}
  \item{\code{$tokenize(sourceDir, targetDir,
  with = "stanfordNLP", lang = "de", ...)}}{Tokenize files in
  \code{sourceDir}, and save results to \code{targetDir}. The result will be
  a verticalized format that can be used for the TreeTagger.} 
  \item{\code{$tokenizeSentences(sourceDir = "xml",targetDir="xmlAnno",
  targetElement = "p", para = FALSE, ...)}}{Use the NLTK sentence tokenizer.}
  \item{\code{$treetagger(sourceDir = "tok", targetDir = "vrt", lang = "de",
  ...)}}{Annotate all files in \code{sourceDir} using treetagger, and save
  results to \code{targetDir}.}
  \item{\code{$fix(sourceDir, targetDir,
  encoding = "UTF-8", replacements = list(), ...)}}{Check files in 
  \code{sourceDir} for potential hickups they may cause, and save output with
  error corrections to \code{targetDir}.} 
  \item{\code{$sAttributeList(sourceDir, sample = 100, ...)}}{Analyse
  structure of XML and return list describing this structure.} 
  \item{\code{$getNestedElements(sourceDir, corpus, element, max.embedding =
  NULL)}}{Helper methode to detect errors in XML documents where cwb-encode
  will throw an error because elements are nested.}
}
}

\section{Arguments}{

\describe{
  \item{dir}{the pipe directory}
  \item{threads}{an integer, the number of threads to use}
  \item{sourceDir}{a subdirectory of the pipeDir where files to be processed 
  reside}
  \item{targetDir}{a subdirectory of the pipeDir where processed output is
  stored}
  \item{ignore.extension}{logical, whether to remove file extensions before
checking whether files in \code{sourceDir} are present in \code{targetDir}}
  \item{corpus}{name of the CWB corpus to create}
}
}

\keyword{datasets}
