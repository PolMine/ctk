java -cp /opt/saxon/saxon9he.jar net.sf.saxon.Transform -t \
  -s:/Users/blaette/Lab/tmp/pipe/xml/BT_15_018.xml \
  -xsl:/Users/blaette/Lab/gitlab/ctk.plpr/inst/xsl/plpr.xsl \
  -o:/Users/blaette/Lab/tmp/pipe/xml2/BT_15_018.xml


java -cp "/Library/Frameworks/R.framework/Versions/3.2/Resources/library/coreNLP/extdata/stanford-corenlp-full-2015-12-09/*" \
  -Xmx4g edu.stanford.nlp.pipeline.StanfordCoreNLP \
  -props /Library/Frameworks/R.framework/Versions/3.2/Resources/library/coreNLP/extdata/StanfordCoreNLP-german.properties \
  -nthreads 1 \
  -outputFormat conll \
  -outputDirectory /Users/blaette/Lab/tmp/pipe/xml3 \
  -file /Users/blaette/Lab/tmp/pipe/xml2/BT_15_018.xml
