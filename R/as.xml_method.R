#' convert data.frame to XML
#'  
#' @param .Object object to convert (data.frame)
#' @param meta character string, the column names of the metadata
#' @param body character string, the column names of the text body, the column names will be turned into the value of a type attribute
#' @param root name of the root node
#' @param element the name no the element to which the paragraphs will be attached
#' @param file filename including the path to which the XML will be written
#' @param clean logical, if TRUE, whitespace will be removed from all columns
#' @param verbose logical
#' @name as.xml
#' @rdname as.xml-method 
#' @exportMethod as.xml
setGeneric("as.xml", function(.Object, ...) standardGeneric("as.xml"))

#' @rdname as.xml-method 
setMethod("as.xml", "data.frame", function(.Object, meta, body, file, root="collection", element="text", clean=TRUE, verbose=TRUE){
  if (clean == TRUE){
    if (clean == TRUE) message("... removing whitespace")
    .clean <- function(x) gsub("^\\s*(.*?)\\s*$", "\\1", x)
    .Object <- t(apply(.Object, 1, .clean))
  }
  if (verbose == TRUE) message("... creating nodes")
  nodes <- lapply(
    c(1:nrow(.Object)),
    function(i) {
      xmlNode(
        element,
        .children=lapply(
          body,
          function(x) xmlNode("p", .children=list(xmlTextNode(.Object[i,x])), attrs=setNames(x, "type"))
          ),
        attrs=.Object[i,meta]
        )
    })
  if (verbose == TRUE) message("... writing XML")
  saveXML(
    xmlNode("root", .children=nodes), file=file,
    prefix = '<?xml version="1.0" encoding="utf-8"?>\n', encoding="UTF-8"
  )
})

#' @import XML
#' @rdname as.xml-method
setMethod("as.xml", "list", function(.Object, filename=NULL){
  doc <- xmlParse("<text></text>", useInternalNodes=T, asText=T)
  docRoot <- xmlRoot(doc)
  xmlAttrs(docRoot) <- .Object[["meta"]]
  dummy <- lapply(
    c(1:length(.Object[["body"]])),
    function(i){
      addChildren(docRoot, newXMLNode("p", attrs=c(type=names(.Object[["body"]])[i]), .Object[["body"]][i]))
    })
  xmlRaw <- saveXML(
    doc, prefix='<?xml version="1.0" encoding="UTF-8"?>\n',
    indent=T, encoding="UTF-8"
  )
  xmlDoc <- unlist(strsplit(xmlRaw, "\\n\\s*"))
  if (is.null(filename)){
    return(xmlDoc)
  } else {
    cat(xmlDoc, file=filename)
    return(NULL)
  }
})