
#' @importFrom readr read_lines write_lines locale
#' @import stringi
.CoNLL2vrt <- function(filename, sourceDir = NULL, targetDir = NULL, verbose = FALSE, param = list()){
  startTime <- Sys.time()
  lines <- readr::read_lines(file = file.path(sourceDir, filename), locale = locale(encoding = "UTF-8"))
  replacements <- list(
    c(pattern = "^\\d+\\t", replacement = ""),
    c(pattern = "\\t_\\t_$", replacement = ""),
    c(pattern = "^(<.*?>).*?$", replacement = "$1"),
    c(pattern = "\xC2\xA0", replacement = " "),
    c(pattern = "&", replacement = "&amp;"),
    c(pattern = "\u00A0", replacement = " "), # 
    c(pattern = "<\\t", replacement = "st\t")
  )
  for (i in 1:length(replacements)){
    lines <- stri_replace_all_regex(
      lines,
      replacement = replacements[[i]]["replacement"],
      pattern = replacements[[i]]["pattern"]
      )
  }
  selfclosingtags <- grep("^<.*/>$", lines)
  blanklines <- grep("^\\s*$", lines)
  linesToRemove <- unique(c(selfclosingtags, blanklines))
  if (length(linesToRemove) > 0) lines <- lines[-linesToRemove]
  filename_out <- gsub("\\..*?$", ".vrt", filename)
  readr::write_lines(x = lines, path = file.path(targetDir, filename_out))
  Sys.time() - startTime
}
