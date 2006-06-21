<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet version="1.0"
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="html"
            omit-xml-declaration="no"
            encoding="iso-8859-1"
            doctype-public="-//W3C//DTD HTML 4.0 Transitional//EN"/>

<xsl:param name="sel_month"  select="'7'"/>
<xsl:param name="sel_year"   select="'2004'"/>
<xsl:param name="month_name" select="'Jul'"/>

<xsl:template match="/">
<html>
  <body>
     <h1>
        <xsl:value-of 
             select="concat('tiOPF-Users ', $month_name, ' ', $sel_year, ' mails')"/>
        </h1>
    <hr />
      <a href="date.htm">[Index By Date]</a>
      <xsl:text> </xsl:text>
      <a href="authors.htm">[Index By Author]</a>
      <xsl:text> </xsl:text>
      <a href="threads.htm">[Index By Thread]</a>
    <hr />
      <ul>
      <xsl:for-each select="//row_index[(Month=$sel_month) and (Year=$sel_year)]">
        <xsl:sort select="concat(substring(Date,  7, 4),
                                 substring(Date,  4, 2),
                                 substring(Date,  1, 2),
                                 substring(Date, 12, 2),
                                 substring(Date, 15, 2),
                                 substring(Date, 18, 2))"
                  data-type="text"/>
        <li><xsl:value-of select="concat('[ ', position(), ' ] ')"/>
          <xsl:element name="a">
            <xsl:attribute name="href">
              <xsl:value-of select="concat(Id, '.htm')"/>
              </xsl:attribute>
            <xsl:value-of select="Subject"/>
            </xsl:element>
          <xsl:text>  </xsl:text>
          <em><xsl:value-of select="concat(eMail, ' (', $month_name, ' ', Day, ')')"/></em> 
        </li>
      </xsl:for-each>
      </ul>
    <hr />
  </body>
</html>
</xsl:template>

</xsl:stylesheet>
