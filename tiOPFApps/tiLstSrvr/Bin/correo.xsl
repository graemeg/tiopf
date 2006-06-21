<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet version="1.0"
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:include href="header.xsl"/>

<xsl:output method="html"
            omit-xml-declaration="no"
            encoding="iso-8859-1"
            doctype-public="-//W3C//DTD HTML 4.0 Transitional//EN"/>

<xsl:template match="/">
<xsl:call-template name="page_header">
<xsl:with-param name="content_tree">
    <ul>
      <li><b>Subject:</b><xsl:value-of select="//subject"/></li>
      <li><b>Sender</b><xsl:text>: </xsl:text>
        <xsl:call-template name="replace-string">
          <xsl:with-param name="text">
            <xsl:call-template name="replace-string">
              <xsl:with-param name="text" select="//sender"/>
              <xsl:with-param name="from" select="'.'"/>
              <xsl:with-param name="to"   select="' dot '"/>
              </xsl:call-template>
            </xsl:with-param>
          <xsl:with-param name="from" select="'@'"/>
          <xsl:with-param name="to"   select="' at '"/>
          </xsl:call-template></li>
      <li><b>Date</b><xsl:text>: </xsl:text><xsl:value-of select="//date"/></li>
    </ul>
    <hr />
    <pre>
    <xsl:call-template name="replace-string">
      <xsl:with-param name="text" select="//body"/>
      <xsl:with-param name="from" select="'@'"/>
      <xsl:with-param name="to"   select="' at '"/>
    </xsl:call-template>
    </pre>
    <hr />
</xsl:with-param>
</xsl:call-template>
</xsl:template>

<!-- reusable replace-string function 
     from: http://aspn.activestate.com/ASPN/Cookbook/XSLT/Recipe/65426
     author: Paul Prescod -->

<xsl:template name="replace-string">
   <xsl:param name="text"/>
   <xsl:param name="from"/>
   <xsl:param name="to"/>

   <xsl:choose>
     <xsl:when test="contains($text, $from)">

	<xsl:variable name="before" select="substring-before($text, $from)"/>
	<xsl:variable name="after" select="substring-after($text, $from)"/>
	<xsl:variable name="prefix" select="concat($before, $to)"/>

	<xsl:value-of select="$before"/>
	<xsl:value-of select="$to"/>
        <xsl:call-template name="replace-string">
	  <xsl:with-param name="text" select="$after"/>
	  <xsl:with-param name="from" select="$from"/>
	  <xsl:with-param name="to" select="$to"/>
	</xsl:call-template>
     </xsl:when> 
     <xsl:otherwise>
       <xsl:value-of select="$text"/>  
     </xsl:otherwise>
   </xsl:choose>            
</xsl:template>

</xsl:stylesheet>
