.Object <- taz
sourceDir <- "vrt"
library(XML)

getAttributeValues(.Object, sourceDir = "vrt", pattern = "vrt", element = "text", attrs = "date", unique = FALSE, progress = TRUE)
