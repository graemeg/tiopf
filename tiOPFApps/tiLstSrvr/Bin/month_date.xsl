<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet version="1.0"
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="html"
            omit-xml-declaration="no"
            encoding="iso-8859-1"
            doctype-public="-//W3C//DTD HTML 4.0 Transitional//EN"/>

<xsl:param name="sel_month"  select="'7'"/>
<xsl:param name="sel_year"   select="'2002'"/>
<xsl:param name="month_name" select="'jul'"/>

<xsl:template match="/">
<html>
  <body>
      <h1>
        <xsl:value-of 
             select="concat('tiOPF-Users ', $month_name, ' ', $sel_year, ' by date')"/>
        </h1>
      <hr />
        <a href="date.htm">[Index By Date]</a>
	<xsl:text> </xsl:text>
	<a href="authors.htm">[Index By Author]</a>
	<xsl:text> </xsl:text>
	<a href="threads.htm">[Index By Thread]</a>
      <hr />
      <xsl:for-each select="//item_DiasMes">
        <xsl:sort select="." data-type="number"/>
        <xsl:variable name="cur_day" select="."/>
	<xsl:variable name="cta" 
	              select="count(//row_index[(Day=$cur_day) and (Month=$sel_month) and (Year=$sel_year)])"/>
        <xsl:if test="not(0 = $cta)">
        <p><xsl:value-of select="concat($month_name, ' ', ., ' (', $cta, ')')"/>
          <ul>
            <xsl:for-each select="//row_index[($cur_day=Day) and ($sel_month=Month) and ($sel_year=Year)]">
              <li>
                <xsl:element name="a">
                  <xsl:attribute name="href">
                    <xsl:value-of select="concat(Id, '.htm')"/>
                    </xsl:attribute>
                  <xsl:value-of select="Subject"/>
                </xsl:element>
                <xsl:text>  </xsl:text>
                <em><xsl:value-of select="concat($month_name, ' ', Day, ' (', eMail, ')')"/></em> 
              </li>
            </xsl:for-each>
          </ul>
        </p>
        </xsl:if>
      </xsl:for-each>
    <hr />
  </body>
</html>
</xsl:template>

</xsl:stylesheet>
