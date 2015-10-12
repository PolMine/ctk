#!/bin/sh

sourcedir="/home/blaette/korpora/plpr/tagged/plpr"
origindir="/home/blaette/korpora/plpr/original"
targetdir="/home/blaette/korpora/plpr/empty"

for sourcefile in $(find $sourcedir -empty | sed 's#\(/.*/\)\([A-Z].*\)\.vrt$#\2#g'); do
	xmlfile=$sourcefile'.xml'
	echo $xmlfile
	mv $origindir/$xmlfile $targetdir 
done;


