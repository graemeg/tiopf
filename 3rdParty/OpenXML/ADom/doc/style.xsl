<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html"/>
  <xsl:strip-space elements="*"/>
  <xsl:preserve-space elements="literallayout programlisting"/>
  
   <xsl:template match="*">
      <xsl:value-of select="."/>
      <xsl:apply-templates/>
   </xsl:template>

   <xsl:template match="text()">
      <xsl:value-of select="."/>
   </xsl:template>

   <xsl:template match="/">
      <HTML>
         <HEAD>
            <TITLE>
               <xsl:value-of select="book/bookinfo/title"/>
            </TITLE>
<STYLE >
BODY {
	BACKGROUND: white fixed no-repeat left top; COLOR: black; FONT-FAMILY: sans-serif; MARGIN: 2em 1em 2em 20px
}
TH {
	FONT-FAMILY: sans-serif
}
TD {
	FONT-FAMILY: sans-serif
}
H1 {
	TEXT-ALIGN: left
}
H2 {
	TEXT-ALIGN: left
}
H3 {
	TEXT-ALIGN: left
}
H4 {
	TEXT-ALIGN: left
}
H5 {
	TEXT-ALIGN: left
}
H6 {
	TEXT-ALIGN: left
}
H1 {
	COLOR: #005a9c
}
H2 {
	COLOR: #005a9c
}
H3 {
	COLOR: #005a9c
}
H4 {
	COLOR: #005a9c
}
H5 {
	COLOR: #005a9c
}
H1 {
	FONT: 170% sans-serif
}
H2 {
	FONT: 140% sans-serif
}
H3 {
	FONT: 120% sans-serif
}
H4 {
	FONT: 100% sans-serif
}
H5 {
	FONT: italic 100% sans-serif
}
H6 {
	FONT: small-caps 100% sans-serif
}
.hide {
	DISPLAY: none
}
DIV.head {
	MARGIN-BOTTOM: 1em
}
DIV.head H1 {
	CLEAR: both; MARGIN-TOP: 2em
}
DIV.head TABLE {
	MARGIN-LEFT: 2em; MARGIN-TOP: 2em
}
DIV.head IMG {
	BORDER-BOTTOM: medium none; BORDER-LEFT: medium none; BORDER-RIGHT: medium none; BORDER-TOP: medium none; COLOR: white
}
P.copyright {
	FONT-SIZE: small
}
P.copyright SMALL {
	FONT-SIZE: small
}
@media Screen    
{
A:hover {
	BACKGROUND: #ffa
}
    }
PRE {
	MARGIN-LEFT: 2em
}
DT {
	MARGIN-BOTTOM: 0px; MARGIN-TOP: 0px
}
DD {
	MARGIN-BOTTOM: 0px; MARGIN-TOP: 0px
}
DT {
	FONT-WEIGHT: bold
}
PRE {
	FONT-FAMILY: monospace
}
CODE {
	FONT-FAMILY: monospace
}
UL.toc {
	LIST-STYLE: none
}

@media Aural    
{
H1 {
	stress: 20; richness: 90
}
H2 {
	stress: 20; richness: 90
}
H3 {
	stress: 20; richness: 90
}
H4 {
	stress: 20; richness: 90
}
.hide {
	speak: none
}
P.copyright {
	volume: x-soft; speech-rate: x-fast
}
DT {
	pause-before: 20%
}
PRE {
	speak-punctuation: code
}
    }
</STYLE>
         </HEAD>
          <BODY STYLE="font:9pt Verdana">
            <xsl:for-each select="book">
               <H1>
                  <xsl:value-of select="bookinfo/titleabbrev"/>
               </H1>
               <H1>
			      <xsl:value-of select="bookinfo/title"/>
               </H1>
               <H1>
			      <xsl:value-of select="bookinfo/subtitle"/>
               </H1>
               <H4>
                  <xsl:value-of select="bookinfo/author/firstname"/>
                  <xsl:text> </xsl:text>
                  <xsl:value-of select="bookinfo/author/surname"/>
               </H4>
               <H4>
                  <xsl:value-of select="bookinfo/pubdate"/>
               </H4>
            </xsl:for-each>

        <hr/>
        <H2>Table of Contents</H2>

        <xsl:apply-templates select="book/preface" mode="toc"/>
        <xsl:apply-templates select="book/chapter" mode="toc"/>
        <xsl:apply-templates select="book/bibliography" mode="toc"/>

        <hr/>
        <xsl:apply-templates/>
        </BODY>
      </HTML>
   </xsl:template>

  <!-- Table of Contents templates -->
  <xsl:template match="preface" mode="toc">
    <DIV STYLE="margin-left:1em">
      <a>
      <xsl:attribute name="href">
         <xsl:text>#preface</xsl:text>
      </xsl:attribute>
      <xsl:value-of select="title"/>
      </a>
    </DIV>
  </xsl:template>
  <xsl:template match="chapter|sect1|sect2|sect3|sect4|sect5" mode="toc">
    <DIV STYLE="margin-left:1em">
      <a>
      <xsl:attribute name="href">
        <xsl:text>#</xsl:text>
        <xsl:apply-templates select="." mode="sectionNum"/><!--<xsl:eval>sectionNum(this)</xsl:eval>-->
      </xsl:attribute>
      <xsl:apply-templates select="." mode="sectionNum"/><!--<xsl:eval>sectionNum(this)</xsl:eval>-->
      <xsl:text> </xsl:text>
      <xsl:value-of select="title"/>
      </a>
      <xsl:apply-templates select="sect1|sect2|sect3|sect4|sect5" mode="toc"/>
    </DIV>
  </xsl:template>
  <xsl:template match="bibliography" mode="toc">
    <DIV STYLE="margin-left:1em">
      <a>
      <xsl:attribute name="href">
        <xsl:text>#bibliography</xsl:text>
      </xsl:attribute>
      <xsl:value-of select="title"/>
      </a>
    </DIV>
  </xsl:template>

   <xsl:template match="book">
     <xsl:apply-templates />
   </xsl:template>
   <xsl:template match="author" />
   <xsl:template match="titleabbrev" />
   <xsl:template match="title" />
   <xsl:template match="subtitle" />
   <xsl:template match="pubdate" />
   <xsl:template match="bookinfo">
     <xsl:apply-templates />
     <hr /> 
   </xsl:template>
   <xsl:template match="legalnotice">
      <h3>Notice</h3>
      <xsl:apply-templates/>
   </xsl:template>
   <xsl:template match="othercredit">
      <h3>Credit</h3>
      <p><xsl:value-of select="contrib"/></p>
      <xsl:apply-templates/>
   </xsl:template>
   <xsl:template match="contrib"/>
   <xsl:template match="chapter">
      <h2>
          <a> 
            <xsl:attribute name="name">
              <xsl:apply-templates select="." mode="sectionNum"/><!--<xsl:eval>sectionNum(this)</xsl:eval>-->
            </xsl:attribute>
            <xsl:apply-templates select="." mode="sectionNum"/><!--<xsl:eval>sectionNum(this)</xsl:eval>-->
            <xsl:text> </xsl:text>
            <xsl:value-of select="title"/>
          </a>
      </h2>
      <xsl:apply-templates/>
   </xsl:template>
   <xsl:template match="sect1">
      <h3>
          <a> 
            <xsl:attribute name="name">
              <xsl:apply-templates select="." mode="sectionNum"/><!--<xsl:eval>sectionNum(this)</xsl:eval>-->
            </xsl:attribute>
            <xsl:apply-templates select="." mode="sectionNum"/><!--<xsl:eval>sectionNum(this)</xsl:eval>-->
            <xsl:text> </xsl:text>
            <xsl:value-of select="title"/>
          </a>         
      </h3>
      <xsl:apply-templates/>
   </xsl:template>
   <xsl:template match="sect2">
      <h4>
          <a> 
            <xsl:attribute name="name">
              <xsl:apply-templates select="." mode="sectionNum"/><!--<xsl:eval>sectionNum(this)</xsl:eval>-->
            </xsl:attribute>
            <xsl:apply-templates select="." mode="sectionNum"/><!--<xsl:eval>sectionNum(this)</xsl:eval>-->
            <xsl:text> </xsl:text>
            <xsl:value-of select="title"/>
          </a>
      </h4>
      <xsl:apply-templates/>
   </xsl:template>
   <xsl:template match="sect3">
      <h5>
          <a> 
            <xsl:attribute name="name">
              <xsl:apply-templates select="." mode="sectionNum"/><!--<xsl:eval>sectionNum(this)</xsl:eval>-->
            </xsl:attribute>
            <xsl:text> </xsl:text>
            <xsl:value-of select="title"/>
          </a>
      </h5>
      <xsl:apply-templates/>
   </xsl:template>
   <xsl:template match="itemizedlist">
      <div class="{name(.)}">
         <xsl:if test="title">
            <xsl:apply-templates select="title"/>
         </xsl:if>
         <ul>
            <xsl:apply-templates select="listitem"/>
         </ul>
      </div>
   </xsl:template>
   <xsl:template match="itemizedlist/title">
      <p>
         <b>
            <xsl:apply-templates/>
         </b>
      </p>
   </xsl:template>
   <xsl:template match="variablelist">
      <div class="{name(.)}">
         <xsl:if test="title">
            <xsl:apply-templates select="title"/>
         </xsl:if>
         <dl>
            <xsl:apply-templates select="varlistentry"/>
         </dl>
      </div>
   </xsl:template>
   <xsl:template match="variablelist/title">
      <p>
         <b>
            <xsl:apply-templates/>
         </b>
      </p>
   </xsl:template>
   <xsl:template match="listitem">
      <li>
         <a name="{generate-id()}"/>
         <xsl:apply-templates/>
      </li>
   </xsl:template>

   <xsl:template match="quote">
      <xsl:text>"</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>"</xsl:text>
   </xsl:template>

   <xsl:template match="para">
      <p><xsl:apply-templates/></p>
   </xsl:template>

<xsl:template match="literallayout">
   <pre class="{name(.)}" style="font:9pt Courier"><xsl:apply-templates/></pre>
</xsl:template>

<xsl:template match="programlisting">
   <pre class="{name(.)}" style="font:9pt Courier"><xsl:apply-templates/></pre>
</xsl:template>

<xsl:template match="synopsis">
   <p style="font:100% Courier"><xsl:apply-templates/></p>
</xsl:template>

<xsl:template match="errorname"><xsl:apply-templates/></xsl:template>

<xsl:template match="emphasis">
      <b>
         <xsl:apply-templates/>
      </b>
</xsl:template>



<!-- Preface -->

<xsl:template match="preface">
  <h2>
    <a> 
      <xsl:attribute name="name">preface</xsl:attribute>
      <xsl:value-of select="title"/>
    </a>
  </h2>
  <xsl:apply-templates/>
</xsl:template>



<!-- Bibliography -->

<xsl:template match="bibliography">
  <h2>
    <a> 
      <xsl:attribute name="name">bibliography</xsl:attribute>
      <xsl:value-of select="title"/>
    </a>
  </h2>
  <ul>
  <xsl:apply-templates/>
  </ul>
</xsl:template>

<xsl:template match="bibliomixed">
  <li><xsl:apply-templates/></li>
</xsl:template>




<!-- Section numbering -->

<xsl:template match="*" mode="sectionNum">
  <xsl:apply-templates select="parent::*" mode="sectionNum"/>
</xsl:template>
<xsl:template match="chapter|sect1|sect2|sect3|sect4|sect5" mode="sectionNum">
  <xsl:apply-templates select="parent::*" mode="sectionNum"/>
  <xsl:number/>
  <xsl:text>.</xsl:text>
</xsl:template>

</xsl:stylesheet>
