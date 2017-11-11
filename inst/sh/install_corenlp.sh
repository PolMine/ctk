#!/bin/sh

mkdir /opt/stanford-corenlp
cd /opt/stanford-corenlp
wget http://nlp.stanford.edu/software/stanford-corenlp-full-2017-06-09.zip
unzip stanford-corenlp-full-2017-06-09.zip
rm stanford-corenlp-full-2017-06-09.zip
cd stanford-corenlp-full-2017-06-09
wget http://nlp.stanford.edu/software/stanford-german-corenlp-2017-06-09-models.jar
zip -d stanford-german-corenlp-2017-06-09-models.jar StanfordCoreNLP-german.properties