<?xml version="1.0" encoding="UTF-8" ?>

<!-- New document created with EditiX at Fri Apr 05 21:50:25 CEST 2013 -->

<xsl:stylesheet version="2.0" 
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
	xmlns:xs="http://www.w3.org/2001/XMLSchema"
	xmlns:fn="http://www.w3.org/2005/xpath-functions"
	xmlns:xdt="http://www.w3.org/2005/xpath-datatypes"
	xmlns:err="http://www.w3.org/2005/xqt-errors"
	exclude-result-prefixes="xs xdt err fn">

	<xsl:output method="xml" indent="yes"/>
	
	<xsl:template match="/">
		<xsl:apply-templates select="/TEI/text/body"/>
	</xsl:template>
	
	<xsl:template match="body">
		<xsl:element name="text">
			<xsl:attribute name="id">1</xsl:attribute>
			<xsl:attribute name="author" select="/TEI/teiHeader/fileDesc/titleStmt/author/text()"/>
			<xsl:attribute name="newspaper" select="/TEI/teiHeader/fileDesc/publicationStmt/publisher/text()"/>
			<xsl:attribute name="date" select="/TEI/teiHeader/fileDesc/publicationStmt/date/@when"/>
			<xsl:copy-of select="./text()"></xsl:copy-of>
		</xsl:element>
	</xsl:template>

</xsl:stylesheet>
