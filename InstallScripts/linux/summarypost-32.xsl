<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" encoding="UTF-8"/>

<xsl:template match="/">

<xsl:text>From: Linux Daily Build &#60;dailybuild&#64;spamfilter.co.za&#62;&#10;</xsl:text>
<xsl:text>Subject: Linux 32-bit Build Status (sha1:#REV) - </xsl:text>
  <xsl:value-of select="/TestResults/DateTimeRan"/><xsl:text>&#10;</xsl:text>
<xsl:text>Newsgroups: tiopf.dailybuilds&#10;</xsl:text>
<xsl:text>Content-Type: text/plain; charset=ISO-8859-1&#10;</xsl:text>
<xsl:text>Content-Transfer-Encoding: 8bit&#10;</xsl:text>
<xsl:text>&#10;</xsl:text>
<xsl:text>               tiOPF2 - Unit Test Results&#10;</xsl:text>
<xsl:text>               --------------------------&#10;</xsl:text>
<xsl:text>&#10;</xsl:text>

Git commit:
   #REV

<xsl:call-template name="build_time"/>
<xsl:call-template name="elapsed_time"/>
Persistence Layers Tested:
   CSV
   TAB
   XMLLight
   FBLIB
   SqlDB - Firebird

<xsl:call-template name="summary"/>

<xsl:text>Details:&#10;</xsl:text>
<xsl:text>  http://geldenhuys.co.uk/tiopf/unittests/linux32.html&#10;</xsl:text>
<xsl:text>&#10;</xsl:text>

</xsl:template>


<xsl:template name="build_time">
<xsl:text>Build time:&#10;</xsl:text>
<xsl:text>   </xsl:text><xsl:value-of select="/TestResults/DateTimeRan"/>
<xsl:text>&#10;&#10;</xsl:text>
</xsl:template>


<xsl:template name="elapsed_time">
<xsl:text>Elapsed time:&#10;</xsl:text>
<xsl:text>   </xsl:text><xsl:value-of select="/TestResults/TotalElapsedTime"/>
<xsl:text>&#10;&#10;</xsl:text>
</xsl:template>


<xsl:template name="summary">
<xsl:text>Summary:&#10;</xsl:text>
<xsl:text>   </xsl:text>FPC #FPCVER (#FPCCPU) - Tests run: <xsl:value-of select="/TestResults/NumberOfRunTests"/>, Failures: <xsl:value-of select="/TestResults/NumberOfFailures"/>, Errors: <xsl:value-of select="/TestResults/NumberOfErrors"/>
<xsl:text>&#10;&#10;</xsl:text>
<xsl:text>   Note:
    FAILURES are anticipated and checked for with assertions.
    ERRORS are unexpected results.</xsl:text>
<xsl:text>&#10;&#10;</xsl:text>
</xsl:template>

</xsl:stylesheet>
