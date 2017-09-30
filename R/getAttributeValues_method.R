#' get attribute values
#' 
#' Get values of attributes of a XML node in files. 
#' @param .Object a character string providing the directory
#' @param element character string length 1 giving the element name
#' @param attrs character string providing attribute names
#' @param unique logical
#' @param pattern e.g. "xml"
#' @param mc logical
#' @param progress logical
#' @param verbose logical
#' @param sample defaults to NULL, or a numeric
#' @param filenames a set of filenames
#' @return If attrs is a character vector of length 1, a character vector, a matrix otherwise
#' @import XML
#' @export getAttributeValues
getAttributeValues <- function(
  .Object, element, attrs, unique=TRUE, pattern = "xml",
  mc = FALSE, progress = TRUE, verbose = FALSE, sample = FALSE, filenames = NULL
  ){
  .getAttributes <- function(filename, sourceDir, targetDir = NULL, verbose = NULL, param){
    element <- param[["element"]]
    attrs <- param[["attrs"]]
    doc <- xmlTreeParse(file.path(sourceDir, filename), useInternalNodes=TRUE)
    nodes <- getNodeSet(doc, paste("//", element))
    attributeValues <- xmlApply(nodes, function(x) xmlAttrs(x)[attrs])
    if (length(attrs) == 1){
      attributeValues <- unique(unname(unlist(attributeValues)))
    } else {
      attributeValues <- do.call(rbind, attributeValues)
    }
    attributeValues
  }
  attributeValues <- dirApply(
    f = .getAttributes, sourceDir = .Object, targetDir = NULL, param = list(element = element, attrs = attrs),
    pattern = pattern, mc = mc, progress = progress, verbose = verbose, sample = sample, filenames = filenames
    )
  if (unique == TRUE){
    if (length(attrs) == 1){
      retval <- unique(unlist(attributeValues))
    } else {
      attributeValuesMatrix <- do.call(rbind, attributeValues)
      retval <- do.call(
        rbind,
        strsplit(unique(apply(attributeValuesMatrix, 1, function(x) paste(x, collapse="||"))), "\\|\\|")
        )
      colnames(retval) <- attrs
    }
  } else {
    if (length(attrs) == 1){
      retval <- unlist(attributeValues)
    } else {
      retval <- do.call(rbind, attributeValues)
    }
  }
  retval
}
