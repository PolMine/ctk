#!/usr/bin/python;

import re, os, codecs
import sys
import xml.dom.minidom
import nltk.data

def sAnnotation(node, doc):
	if node.hasChildNodes():
		if node.childNodes[0].nodeType == 3:
			saetze = tokenizer.tokenize(node.childNodes[0].data)
			node.replaceChild(doc.createElement('s'), node.childNodes[0])
			node.childNodes[0].appendChild(doc.createTextNode(saetze[0]))
			if len(saetze) > 0:
				for j in range(1,len(saetze)):
					node.appendChild(doc.createElement('s'))
					node.childNodes[-1].appendChild(doc.createTextNode(saetze[j]))
	return(doc)


def psAnnotation(node, doc):
	if node.hasChildNodes():
		for i in range(len(node.childNodes)):
			if node.childNodes[i].nodeType == 3:
				para = node.replaceChild(doc.createElement('p'), node.childNodes[i])
				saetze = tokenizer.tokenize(para.data)
				if len(saetze) > 0:
					for satz in saetze:
						node.childNodes[i].appendChild(doc.createElement('s'))
						node.childNodes[i].childNodes[-1].appendChild(doc.createTextNode(satz))
	return(doc)


sourceDir = sys.argv[1]
fileIn = sys.argv[2]
dirOut = sys.argv[3]
targetElement = sys.argv[4]
annotateParagraphs = sys.argv[5]

tokenizer = nltk.data.load('tokenizers/punkt/german.pickle')
doc = xml.dom.minidom.parse(os.path.join(sourceDir, fileIn))
nodes = doc.getElementsByTagName(targetElement)
if annotateParagraphs == "TRUE":
	for node in nodes: doc = psAnnotation(node, doc)
if annotateParagraphs == "FALSE":
	for node in nodes: doc = sAnnotation(node, doc)

codecs.open(os.path.join(dirOut, fileIn), 'w', 'utf-8').writelines(doc.toxml())
	
