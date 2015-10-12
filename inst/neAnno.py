#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os, re, csv, codecs, pickle
import PyCQP_interface 
import subprocess

def decode(korpus, filenameTokenStream):
	os.system('cwb-decode -C NW14PDK -P word > %s' % filenameTokenStream)

class CQPRetrieval:
	def __init__(self, korpus):
		self.cqp = PyCQP_interface.CQP(bin='/usr/local/bin/cqp',options='-c')
		print
		self.cqp.Exec('''%s;''' % korpus)

	def getMatchRegions(self, query):
		query = query.encode('latin-1')
		self.cqp.Exec('''%s;''' % query)
		noMatches = int(self.cqp.Exec('size Last;'))
		matchRegions = self.cqp.Dump(first=0, last = noMatches)
		matchRegions = [(begin, end) for (begin, end, anchor, keyword) in matchRegions]
		return(matchRegions)

	def close(self):
		self.cqp.Terminate()
	
def annotate(matchRegions, type, fileIn, fileOut):
	rein = codecs.open(fileIn, 'r', 'latin-1')
	raus = codecs.open(fileOut, 'w', 'latin-1')
	i = 0
	for (neBegin, neEnd) in matchRegions:
		while i < int(neBegin):
			raus.write(rein.readline())
			i += 1
		raus.write('<ne type="%s">' % type)
		print "match"
		while i < int(neEnd):
			raus.write(rein.readline())
			i += 1
		raus.write(rein.readline().rstrip())
		raus.write('</ne>\n')
		i += 1
	while True:
		zeile = rein.readline()
		if not zeile == '':
			raus.write(zeile)
		else:
			break
	rein.close()
	raus.close()

def newline(filename_ne):
	rein = codecs.open(filename_ne, 'r', 'latin-1')
	raus = codecs.open(filename_ne, 'w', 'latin-1')
	while True:
		zeile = rein.readline()
		if not zeile == '':
			zeile = re.sub('>', '>\n', zeile)
			raus.write(zeile)
		else:
			break

def recode(korpus, file_ne):
	os.system('cwb-encode -d /var/local/cwb/indexed_corpora/%s -f %s -p - -S ne:0+type' % (k.lower(), file_ne))
	# os.system('cwb-regedit %s :add :s ne ne_type' % korpus)
	os.system('cwb-make -V %s' % korpus)


korpus = "NW14PDK"
queries = (('''[lemma="Ministerium"][lemma="für"][pos="NN"]''', 'organization'))

filename_tokenStream = "%s_tokenStream.txt" % korpus
filename_ne_tmp = "%s_recode_tmp.txt" % korpus
filename_ne_final = "%s_recode.txt" % korpus

decode(korpus, filenameTokenStream)
engineCQP = CQPRetrieval(korpus)
for (query, type) in queries:
		matchRegions = engineCQP.getMatchRegions(query)
		annotate(matchRegions, type, filename_tokenStream, filename_ne_tmp)
engine.close()
newline(filename_ne_tmp, filename_ne)
recode(korpus, filename_ne) 

