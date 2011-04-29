<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet version="1.0" 
		 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:rss="http://purl.org/rss/1.0/"
         xmlns:dc="http://purl.org/dc/elements/1.1/">
	<xsl:output method="html"/>
	<xsl:variable name="title" select="rdf:RDF/rss:channel/rss:title"/>
	<xsl:template match="/">
		<html>
			<head>
				<title><xsl:value-of select="$title"/></title>
			</head>
			<body>
				<h2 align="center"><xsl:value-of select="$title"/></h2>
				<ul>
					<xsl:apply-templates select="rdf:RDF/rss:item"/>
				</ul>
			</body>
		</html>	
	</xsl:template>
	<xsl:template match="rdf:RDF/rss:item">
		<li><i><a href="{./rss:link}"><xsl:value-of select="./rss:title"/></a></i><br/>
			<xsl:value-of select="./dc:description"/></li>
	</xsl:template>
</xsl:stylesheet>