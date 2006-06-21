<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet version="1.0"
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="html"
            omit-xml-declaration="no"
            encoding="iso-8859-1"
            doctype-public="-//W3C//DTD HTML 4.0 Transitional//EN"/>

<xsl:param name="sel_month"  select="'7'"/>
<xsl:param name="sel_year"   select="'2002'"/>
<xsl:param name="month_name" select="'Jul'"/>

<xsl:template match="/">
<html>
  <body>
      <h1>
        <xsl:value-of 
             select="concat('tiOPF-Users ', $month_name, ' ', $sel_year, ' by author')"/>
        </h1>
      <hr />
        <a href="date.htm">[Index By Date]</a>
        <xsl:text> </xsl:text>
        <a href="authors.htm">[Index By Author]</a>
        <xsl:text> </xsl:text>
        <a href="threads.htm">[Index By Thread]</a>
      <hr />
      <xsl:for-each select="//eMail[not(.=preceding::eMail)]">
        <xsl:sort select="."/>
        <xsl:variable name="cur_author" select="."/>
	<xsl:variable name="cta" 
	              select="count(//eMail[($sel_month=../Month) and ($sel_year=../Year) and ($cur_author=.)])"/>
        <xsl:if test="0 &lt; $cta">
          <p><xsl:value-of 
                  select="concat(., ' (', $cta, ')')"/>
            <ul>
              <xsl:for-each select="//row_index[($cur_author=eMail) and ($sel_month=Month) and ($sel_year=Year)]">
                <xsl:sort select="concat(substring(Date,  7, 4),
                                         substring(Date,  4, 2),
                                         substring(Date,  1, 2),
                                         substring(Date, 12, 2),
                                         substring(Date, 15, 2),
                                         substring(Date, 18, 2))"
                          data-type="text"/>
                <li>
                  <xsl:element name="a">
                    <xsl:attribute name="href">
                      <xsl:value-of select="concat(Id, '.htm')"/>
                      </xsl:attribute>
                    <xsl:value-of select="Subject"/>
                  </xsl:element>
                  <xsl:text>  </xsl:text>
                  <em><xsl:value-of select="concat($month_name, ' ', Day)"/></em>
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
