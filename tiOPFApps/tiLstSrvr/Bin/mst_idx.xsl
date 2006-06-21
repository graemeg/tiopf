<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet version="1.0"
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:include href="page.xsl"/>

<xsl:output method="html"
            omit-xml-declaration="no"
            encoding="iso-8859-1"
            doctype-public="-//W3C//DTD HTML 4.0 Transitional//EN" />

<xsl:template match="/">
 <xsl:call-template name="page_content">
   <xsl:with-param name="archive_table">
    <table border="1">
      <tbody>
        <tr>
          <th>&#160;</th>
	  <th>Jan</th>
	  <th>Feb</th>
	  <th>Mar</th>
	  <th>Apr</th>
	  <th>May</th>
	  <th>Jun</th>
	  <th>Jul</th>
	  <th>Aug</th>
	  <th>Sep</th>
	  <th>Oct</th>
	  <th>Nov</th>
	  <th>Dec</th>
	</tr>
        <xsl:for-each select="//Year[not(.=preceding::Year)]">
          <xsl:sort select="."/>
          <xsl:variable name="cur_year">
            <xsl:value-of select="."/>
            </xsl:variable>
          <xsl:variable name="cur_jan">
	    <xsl:value-of 
                 select="count(//row_index[(Year=$cur_year) and (Month= 1)])"/>
            </xsl:variable>
          <xsl:variable name="cur_feb">
	    <xsl:value-of 
                 select="count(//row_index[(Year=$cur_year) and (Month= 2)])"/>
            </xsl:variable>
          <xsl:variable name="cur_mar">
	    <xsl:value-of 
                 select="count(//row_index[(Year=$cur_year) and (Month= 3)])"/>
            </xsl:variable>
          <xsl:variable name="cur_apr">
	    <xsl:value-of 
                 select="count(//row_index[(Year=$cur_year) and (Month= 4)])"/>
            </xsl:variable>
          <xsl:variable name="cur_may">
	    <xsl:value-of 
                 select="count(//row_index[(Year=$cur_year) and (Month= 5)])"/>
            </xsl:variable>
          <xsl:variable name="cur_jun">
	    <xsl:value-of 
                 select="count(//row_index[(Year=$cur_year) and (Month= 6)])"/>
            </xsl:variable>
          <xsl:variable name="cur_jul">
	    <xsl:value-of 
                 select="count(//row_index[(Year=$cur_year) and (Month= 7)])"/>
            </xsl:variable>
          <xsl:variable name="cur_aug">
	    <xsl:value-of 
                 select="count(//row_index[(Year=$cur_year) and (Month= 8)])"/>
            </xsl:variable>
          <xsl:variable name="cur_sep">
	    <xsl:value-of 
                 select="count(//row_index[(Year=$cur_year) and (Month= 9)])"/>
            </xsl:variable>
          <xsl:variable name="cur_oct">
	    <xsl:value-of 
                 select="count(//row_index[(Year=$cur_year) and (Month=10)])"/>
            </xsl:variable>
          <xsl:variable name="cur_nov">
	    <xsl:value-of 
                 select="count(//row_index[(Year=$cur_year) and (Month=11)])"/>
            </xsl:variable>
          <xsl:variable name="cur_dec">
	    <xsl:value-of 
                 select="count(//row_index[(Year=$cur_year) and (Month=12)])"/>
            </xsl:variable>

          <tr>
	    
	    <!-- Year -->
            <td align="right">
	      <xsl:choose>
	        <xsl:when test="1899 != $cur_year"><xsl:value-of select="$cur_year"/></xsl:when>
		<xsl:otherwise>&#160;</xsl:otherwise></xsl:choose></td>

	    <!-- January -->
            <td align="right">
	      <xsl:choose>
	        <xsl:when test="0 != $cur_jan">
		  <xsl:element name="a">
		    <xsl:attribute name="href">
		      <xsl:value-of select="concat($cur_year, '/1/default.htm')"/></xsl:attribute>
		    <xsl:value-of select="$cur_jan"/></xsl:element></xsl:when>
		<xsl:otherwise>&#160;</xsl:otherwise></xsl:choose></td>
	    
	    <!-- February -->
            <td align="right">
	      <xsl:choose>
	        <xsl:when test="0 != $cur_feb">
		  <xsl:element name="a">
		    <xsl:attribute name="href">
		      <xsl:value-of select="concat($cur_year, '/2/default.htm')"/></xsl:attribute>
		    <xsl:value-of select="$cur_feb"/></xsl:element></xsl:when>
		<xsl:otherwise>&#160;</xsl:otherwise></xsl:choose></td>

            <!-- March -->
            <td align="right">
	      <xsl:choose>
	        <xsl:when test="0 != $cur_mar">
		  <xsl:element name="a">
		    <xsl:attribute name="href">
		      <xsl:value-of select="concat($cur_year, '/3/default.htm')"/></xsl:attribute>
		    <xsl:value-of select="$cur_mar"/></xsl:element></xsl:when>
		<xsl:otherwise>&#160;</xsl:otherwise></xsl:choose></td>

	    <!-- April -->
            <td align="right">
	      <xsl:choose>
	        <xsl:when test="0 != $cur_apr">
		  <xsl:element name="a">
		    <xsl:attribute name="href">
		      <xsl:value-of select="concat($cur_year, '/4/default.htm')"/></xsl:attribute>
		    <xsl:value-of select="$cur_apr"/></xsl:element></xsl:when>
		<xsl:otherwise>&#160;</xsl:otherwise></xsl:choose></td>

	    <!-- May -->
            <td align="right">
	      <xsl:choose>
	        <xsl:when test="0 != $cur_may">
		  <xsl:element name="a">
		    <xsl:attribute name="href">
		      <xsl:value-of select="concat($cur_year, '/5/default.htm')"/></xsl:attribute>
		    <xsl:value-of select="$cur_may"/></xsl:element></xsl:when>
		<xsl:otherwise>&#160;</xsl:otherwise></xsl:choose></td>

	    <!-- June -->
            <td align="right">
	      <xsl:choose>
	        <xsl:when test="0 != $cur_jun">
		  <xsl:element name="a">
		    <xsl:attribute name="href">
		      <xsl:value-of select="concat($cur_year, '/6/default.htm')"/></xsl:attribute>
		      <xsl:value-of select="$cur_jun"/></xsl:element></xsl:when>
		<xsl:otherwise>&#160;</xsl:otherwise></xsl:choose></td>

	    <!-- July -->
            <td align="right">
	      <xsl:choose>
	        <xsl:when test="0 != $cur_jul">
		  <xsl:element name="a">
		    <xsl:attribute name="href">
		      <xsl:value-of select="concat($cur_year, '/7/default.htm')"/></xsl:attribute>
		      <xsl:value-of select="$cur_jul"/></xsl:element></xsl:when>
		<xsl:otherwise>&#160;</xsl:otherwise></xsl:choose></td>

	    <!-- August -->
            <td align="right">
	      <xsl:choose>
	        <xsl:when test="0 != $cur_aug">
		  <xsl:element name="a">
		    <xsl:attribute name="href">
		      <xsl:value-of select="concat($cur_year, '/8/default.htm')"/></xsl:attribute>
		      <xsl:value-of select="$cur_aug"/></xsl:element></xsl:when>
		<xsl:otherwise>&#160;</xsl:otherwise></xsl:choose></td>

	    <!-- September -->
            <td align="right">
	      <xsl:choose>
	        <xsl:when test="0 != $cur_sep">
		  <xsl:element name="a">
		    <xsl:attribute name="href">
		      <xsl:value-of select="concat($cur_year, '/9/default.htm')"/></xsl:attribute>
		      <xsl:value-of select="$cur_sep"/></xsl:element></xsl:when>
		<xsl:otherwise>&#160;</xsl:otherwise></xsl:choose></td>

	    <!-- October -->
            <td align="right">
	      <xsl:choose>
	        <xsl:when test="0 != $cur_oct">
		  <xsl:element name="a">
		    <xsl:attribute name="href">
		      <xsl:value-of select="concat($cur_year, '/10/default.htm')"/></xsl:attribute>
		      <xsl:value-of select="$cur_oct"/></xsl:element></xsl:when>
		<xsl:otherwise>&#160;</xsl:otherwise></xsl:choose></td>

	    <!-- November -->
            <td align="right">
	      <xsl:choose>
	        <xsl:when test="0 != $cur_nov">
		  <xsl:element name="a">
		    <xsl:attribute name="href">
		      <xsl:value-of select="concat($cur_year, '/11/default.htm')"/></xsl:attribute>
		      <xsl:value-of select="$cur_nov"/></xsl:element></xsl:when>
		<xsl:otherwise>&#160;</xsl:otherwise></xsl:choose></td>

	    <!-- December -->
            <td align="right">
	      <xsl:choose>
	        <xsl:when test="0 != $cur_dec">
		  <xsl:element name="a">
		    <xsl:attribute name="href">
		      <xsl:value-of select="concat($cur_year, '/12/default.htm')"/></xsl:attribute>
		      <xsl:value-of select="$cur_dec"/></xsl:element></xsl:when>
		<xsl:otherwise>&#160;</xsl:otherwise></xsl:choose></td>
          </tr>
        </xsl:for-each>
      </tbody>
    </table>

  </xsl:with-param>
 </xsl:call-template>
</xsl:template>

</xsl:stylesheet>
