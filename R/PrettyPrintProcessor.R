PrettyPrintProcessor <- setRefClass(
  
  "PrettyPrintProcessor",
  
  fields = list(
    
  ),
  
  methods = list(
    
    parsePrettyPrint = function(x = NULL, filename = NULL, mc = 1){
      if (is.null(x)) x <- readLines(filename)
      chunks <- cut(
        1:length(x),
        c(grep("^Sentence\\s#\\d+", x), length(x)),
        include.lowest = TRUE, right = FALSE
      )
      dts <- pbapply:pblapply(
        split(x, f = chunks),
        function(chunk){
          txt <- chunk[grepl("^\\[.*\\]$", chunk)] # get lines with annotation
          regex <- "^.*?Text=(.*?)\\s.*\\sPartOfSpeech=(.*?)\\sNamedEntityTag=(.*?)\\]"
          df <- stringi::stri_match(txt, regex = regex)
          dt <- as.data.table(df)[,2:4, with = FALSE]
          colnames(dt) <- c("token", "pos", "ner")
          dt
        },
        cl = mc
      )
      # rbindlist(dts)
      dts
    }
  )
)

