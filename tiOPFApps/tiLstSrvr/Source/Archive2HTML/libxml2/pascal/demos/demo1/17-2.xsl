<?xml version="1.0"?>
<xsl:stylesheet version="1.0" 
          xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="PERIODIC_TABLE">
    <html>
      <xsl:apply-templates/>
    </html>
  </xsl:template>
  <xsl:template match="ATOM">
    <P>
      <xsl:apply-templates/>
    </P>
  </xsl:template>
</xsl:stylesheet>

