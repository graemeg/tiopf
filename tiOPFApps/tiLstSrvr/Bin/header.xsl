<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet version="1.0"
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="html"
            omit-xml-declaration="no"
            encoding="iso-8859-1"
            doctype-public="-//W3C//DTD HTML 4.0 Transitional//EN" />

<xsl:template name="page_header"><xsl:param name="content_tree"/>

<html>
<body>
<h1>Header.xls</h1>
<hr></hr>
<a href="date.htm">[Index By Date]  </a> 
<a href="authors.htm">[Index By Author]  </a> 
<a href="threads.htm">[Index By Thread]</a>
<xsl:copy-of select="$content_tree"/>
</body>
</html>
</xsl:template>
</xsl:stylesheet>