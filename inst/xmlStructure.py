#!/usr/bin/env python
# -*- coding: utf-8 -*-

import xml.dom.minidom
import re, codecs, subprocess, os


def getElements(node, e):
	nodeCount = 0 
	while True:
		processedNode = node.childNodes[nodeCount]
		if not processedNode.nodeName in e:
			e.append(processedNode.nodeName)
		if processedNode.hasChildNodes() == True:
			getElements(processedNode, e)
			nodeCount += 1
		else:
			nodeCount += 1
		if nodeCount >= len(node.childNodes):
			break
	# e.remove(u"#text")
	return(e)

def getAttributes(doc, elements):
	attributes = {}
	for element in elements:
		occurences = doc.getElementsByTagName(element)
		for occurence in occurences:
			attributesElement = occurence.attributes.keys()
			if not attributes.has_key(element):
				attributes[element] = attributesElement
			else:
				for a in attributesElement:
					if not a in attributes[element]:
						attributes[element].append(a)
	return(attributes)

def encodeCommandSAttributes(attributes):
	s = ''
	for element in attributes.keys():
		if attributes[element] == []:
			toAdd = ' '
		else:
			
			addList = ["+%s" % attribute for attribute in attributes[element]]
			toAdd = ''.join(addList) + ' '
		s += "-S %s%s" % (element, toAdd)
		# kleiner Schoenheitsfehler: Leerzeichen am Ende
	return s

def prepareEncodingS(doc):
	e = []
	elements = getElements(doc, e)
	attributeDir = getAttributes(doc, elements)
	sAttributes = encodeCommandSAttributes(attributeDir)
	return(sAttributes)


def encodeShellCommands(korpus, filename):
	doc = xml.dom.minidom.parse(filename)
	cwb_dir = '/var/local/cwb/'
	dir_inputFiles = '%scorpora/%s' % (cwb_dir, korpus)
	dir_indexedCorpus = '%sindexed_corpora/%s' % (cwb_dir, korpus)
	registry_file = '%sregistry_files/%s' % (cwb_dir, korpus)
	pAttributes = '-P pos -P lemma'
	sAttributes = prepareEncodingS(doc)
	cwbEncode = 'cwb-encode -d %s -F %s -R %s %s %s' % (dir_indexedCorpus, dir_inputFiles, registry_file, pAttributes, sAttributes)
	cwbMake = 'cwb-make -r %sregistry_files -V %s' % (cwb_dir, korpus.upper())
	return(cwbEncode, cwbMake)

filename = '/var/local/cwb/corpora/nw2010spd/nw2010spd.vrt'
korpus = 'nw2010spd'
(cwbEncode, cwbMake) = encodeShellCommands(korpus, filename)
print cwbEncode
print
print cwbMake
os.system(cwbEncode)
# subprocess.call(cwbMake)

