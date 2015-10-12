#!/usr/bin/python
# -*- encoding: utf-8 -*-

import os

inputdir = '/home/data/archive/CWB_Korpus/nw15pi_utf8'
outputdir = '/home/data/archive/CWB_Korpus/nw15pi_latin1'

files = os.listdir(inputdir)
for file in files:
	iconv = "iconv -f utf8 -t latin1 %s -o %s" % (os.path.join(inputdir, file), os.path.join(outputdir,file))
	print iconv
	os.popen(iconv)
