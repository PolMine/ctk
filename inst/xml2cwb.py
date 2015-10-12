#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os, sys
from xml.dom import minidom, Node
import treetaggerwrapper
import codecs
import re
import datetime, time
import shutil
import getpass
import logging, warnings
from socket import gethostname

logging.basicConfig()
warnings.filterwarnings('ignore')

tagger_dir = '/opt/treetagger'

# if not os.geteuid()==0:
#   sys.exit("\n= = = Bitte fuehren Sie das Skript mit sudo aus! = = =\n")

escape = re.compile("\s(?P<nr>([\d|\w]+\.)+[\d|\w]+)\s")
elemente = {}
anzahl_doc = 0
start = datetime.datetime.now()

def correctText(text): # smal corrections for the CWB
    text = text.replace("&lt;", " ")
    text = text.replace("&gt;", " ")
    text = text.replace("<", " ")
    text = text.replace(">", " ")
    text = re.sub("\s+", " ", text)
    text = re.sub("\r?\n", "", text)
    text = text.replace(u"\u2013", '')
    text = text.replace(u"\u201e", '"')
    text = text.replace(u"\u201c", '"')
    text = text.replace(u"\u20ac", 'Euro')
    text = text.replace(u'\u201d', '"')
    text = text.replace(u'\u2019', "'")
    return text

def convert(node, doc): # replace textNodes with verticalized textNodes (tagged with TreeTagger)
    for child in node.childNodes:
        if child.nodeType == Node.TEXT_NODE:
            text = child.nodeValue
            text == correctText(text)

            ### Tagging TextNode
            textNewNode = []
            escaped_id = 0
            escaped = {}
            text = text.encode("utf-8")
            if re.match("[\r\n\s\t]+$", text) == None:
                for no in [x.groupdict() for x in escape.finditer(text)]: # escaping strings from false tagging
                    escaped_id += 1
                    escaped["x%sx" %escaped_id] = no['nr']
                    text = text.replace(no['nr'], "x%sx" %escaped_id)
                tags = tagger.TagText(text, encoding='utf-8')
                for elem in tags:
                    elem = elem.decode("utf-8")
                    tag_list = elem.split('\t')
                    if tag_list[0] in escaped.keys(): # revert escaped strings
                        tag_list[0] = escaped[tag_list[0]]
                        tag_list[1] = u'CARD'
                        tag_list[2] = tag_list[0]
                    if len(tag_list) == 3: # right taggings
                        if tag_list[2] == "<unknown>":
                            tag_list[2] = "#unknown#"
                        textNewNode.append("%s\t%s\t%s\n" %(tag_list[0], tag_list[1], tag_list[2]))
                    else: # false taggings
                        textNewNode.append("%s\t#unknown#\t#unknown#" %tag_list[0])
                newNode = doc.createTextNode("".join(textNewNode))
                child.parentNode.replaceChild(newNode, child)
        else: # if not TextNode
            if (child.nodeName in elemente.keys()) and (child.attributes != None):
                for attribute in child.attributes.keys():
                    if attribute not in elemente[child.nodeName]:
                        elemente[child.nodeName].append(attribute)
            else:
                if child.attributes != None:
                    elemente[child.nodeName] = child.attributes.keys()
                else:
                    elemente[child.nodeName] = []

            if child.attributes != None:
                for (name, value) in child.attributes.items():
                    value = correctText(value)
                    child.setAttribute(name, value)

            if child.nodeName == "party": # replacing party-node with textNode in this way: "token NE party_abbreviation"
                party_text = child.childNodes[0].nodeValue
                party_lemma = child.getAttribute("abbr")
                newNode = doc.createTextNode("%s\tNE\t%s\n" %(party_text, party_lemma))
                child.parentNode.replaceChild(newNode, child)
            else:
                convert(child, doc)
    return doc

def cwb_elements(node):
    for child in node.childNodes:
        if child.nodeType == Node.TEXT_NODE:
            cwb_elements(child)
        else:
            if child.nodeName in elemente.keys():
                for attribute in child.attributes.keys():
                    if attribute not in elemente[child.nodeName]:
                        elemente[child.nodeName].append(attribute)
            else:
                elemente[child.nodeName] = child.attributes.keys()
            cwb_elements(child)

class communication:

    def __init__(self, eingabe="", anzahl=0): # Eingaben-Handling
        self.data = ""
        if not eingabe == "":
            self.frage(eingabe, anzahl)

    def frage(self, eingabe, anzahl=0): # Ausgabe der Eingabe
        anzahl += 1
        self.data = raw_input(eingabe) # Eingabe speicher
        if self.data in ["Ja", "ja", "j", "J"]:
            self.data = "ja"
        elif self.data in ["nein", "Nein", "n", "N"]:
            self.data = "nein"

        if anzahl < 3:
            if self.data == "": # leere Eingaben verhindern
                self.frage(eingabe, anzahl)
            elif "Namen" not in eingabe:
                if self.data not in ["ja", "nein"]:
                    if (("Pfad" in eingabe) or ("Zielpfad" in eingabe)) and (("/" in self.data) or ("\\" in self.data)): # mutmasslicher Pfad
                        pfad_test = os.path.exists(self.data)
                        if pfad_test == False:
                            self.frage("Dieser Pfad existiert nicht! Soll es erstellt werden?")
                    else:
                        print "= = = Die Eingabe ist ungueltig! = = ="
                        self.frage(eingabe, anzahl)
                else:
                    if "CWB_Korpus" in eingabe:
                        if self.data == "nein":
                            self.frage("Bitte geben Sie einen neuen Zielpfad fuer die Daten an?\n", anzahl)
                        elif self.data == "ja":
                            self.data = "/home/data/archive/CWB_Korpus"
        else:
            sys.exit("3 ungueltige Eingaben => das Skript wird beendet")

    def antwort(self): # Nutzereingabe zurueckgeben
        return self.data

def convert_import(quell_pfad, ziel_pfad, korpus_name, konvertieren, importieren, anzahl_doc):
    for root, dirs, files in os.walk(quell_pfad):
        for filename in files:
            if not (filename.endswith(".xml") or filename.endswith(".vrt") or filename.endswith(".tei")): continue

            print filename
            name = filename.split(".")[0]
            try:
              doc = minidom.parse(os.path.join(root, filename).encode("utf-8"))
              if not os.path.exists(ziel_pfad):
                  os.makedirs(ziel_pfad)
              out = codecs.open("%s/%s.vrt" %(ziel_pfad, name), "w", "utf-8")
  
              konv = 0
              if konvertieren == "ja" and filename.endswith(".xml"):
                  anzahl_doc += 1
                  konv = 1
                  if anzahl_doc == 1:
                      print "Die xml-Dokumente werden konvertiert und unter %s gespeichert!" %ziel_pfad
                  doc = convert(doc, doc)
              if importieren == "ja" and filename.endswith(".vrt"):
                  if konv == 0:
                      anzahl_doc += 1
                  if quell_pfad != "%s/%s" %(ziel_pfad, korpus_name):
                      shutil.copyfile("%s/%s" %(quell_pfad, filename), "%s/%s/%s" %(ziel_pfad, korpus_name, filename))
                  if anzahl_doc == 1:
                      print "Die Daten werden in die CWB importiert!"
                  cwb_elements(doc)
            except:
              print filename, "-> Fehler: Datei wurde nicht konvertiert!"

            try:
                out.write(re.sub("(<[^>]+>)", "\\1\n", doc.toxml("utf-8").decode("utf-8")))
            except:
                print filename, "-> Fehler: Datei wurde nicht konvertiert!"
    return anzahl_doc

def import_cwb(ziel_pfad, korpus_name):
    if not os.path.exists("/var/local/cwb/indexed_corpora/%s" %korpus_name):
        os.makedirs("/var/local/cwb/indexed_corpora/%s" %korpus_name)

    cwb_execute = "cwb-encode -d /var/local/cwb/indexed_corpora/%s -F %s/%s -R /var/local/cwb/registry_files/%s -xsB -P pos -P lemma" %(korpus_name, ziel_pfad, korpus_name, korpus_name)
    for elem in elemente.keys():
        if elemente[elem] != []:
            cwb_execute = " ".join([cwb_execute, "-S ", "+".join([elem, "+".join(elemente[elem])])])
        else:
            cwb_execute = " ".join([cwb_execute, "-S ", elem])
    os.popen(cwb_execute)
    cwbMake = "cwb-make -r /var/local/cwb/registry_files -V %s" %korpus_name
    os.popen(cwbMake)


if sys.argv[1] == "-cli":
	konvertieren = "ja"
	importieren = "nein"
	quell_pfad = sys.argv[2]
	ziel_pfad = sys.argv[3]
	korpus_sprache = sys.argv[4]
	korpus_name = sys.argv[5].lower()
	tagger_dir = sys.argv[6]

else:
	konvertieren = communication("Moechten Sie xml-Daten in vrt-Daten konvertieren?\n").antwort()
	importieren = communication("Moechten Sie Daten in die CWB importieren?\n").antwort()
	quell_pfad = ""
	if konvertieren == "ja":
    		quell_pfad = communication("Bitte geben Sie den Pfade zu den XML-DATEN an!\n").antwort()
	else:
	    quell_pfad = communication("Bitte geben Sie den Pfade zu den VRT-DATEN an!\n").antwort()
	korpus_name = communication("Geben Sie bitte einen Namen fuer das Korpus an!\n").antwort()
	korpus_sprache = communication("Was ist die Sprache des Korpus?\n").antwort()
	ziel_pfad = communication("In welchem Verzeichnis sollen die Daten gespeichert werden?").antwort()

tagger = treetaggerwrapper.TreeTagger(TAGLANG=korpus_sprache,TAGDIR=tagger_dir) # Aufruf des TreeTaggers

anzahl_doc = convert_import(quell_pfad, ziel_pfad, korpus_name, konvertieren, importieren, anzahl_doc)
if importieren == "ja":
    import_cwb(ziel_pfad, korpus_name)
print "*** Arbeitsauftrag beendet ***"
print "*** Anzahl der bearbeiteten Dokumente: %s" %anzahl_doc
print "*** Benoetigte Zeit (hh:mm:ss.nanosec): ", datetime.datetime.now()-start
