<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet version="1.0"
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="html"
            omit-xml-declaration="no"
            encoding="iso-8859-1"
            doctype-public="-//W3C//DTD HTML 4.0 Transitional//EN" />


<xsl:template name="page_content"><xsl:param name="archive_table"/>
<html>
<body>
<xsl:copy-of select="$archive_table"/>
</body>
</html>
</xsl:template>
</xsl:stylesheet>