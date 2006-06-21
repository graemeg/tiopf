<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet version="1.0"
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="text"
            omit-xml-declaration="no"
            encoding="iso-8859-1"/>

<xsl:param name="sel_month"  select="'7'"/>
<xsl:param name="sel_year"   select="'2002'"/>

<xsl:template match="/">
  <xsl:value-of 
      select="count(//Day[(../Month=$sel_month) and (../Year=$sel_year)])"/>

<xsl:text>
Lista días de 7/2002
</xsl:text>

  <xsl:for-each select="//Day[not(.=preceding::Day)]">
    <xsl:sort select="." data-type="number"/>
     
    <xsl:variable name="cur_day" select="."/>
    <xsl:variable name="cta_day" select="count(//row_index[(Day=$cur_day) and (Month=$sel_month) and (Year=$sel_year)])"/>

    <xsl:if test="0 &lt; $cta_day">
<xsl:text>
</xsl:text>
    <xsl:value-of select="concat($cur_day, ' ', $sel_month)"/>
<xsl:text>
</xsl:text>

    <xsl:for-each select="//row_index[(Day=$cur_day) and (Month=$sel_month) and (Year=$sel_year)]">
      <xsl:value-of select="concat(Date, ' ', Subject, ' ', eMail )"/>
<xsl:text>
</xsl:text>
    </xsl:for-each>
    </xsl:if>
  </xsl:for-each>
</xsl:template>

</xsl:stylesheet>
