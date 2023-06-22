<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

   <xsl:output method="text"/>

   <xsl:variable name="nomsol">?</xsl:variable>

   <xsl:template match="/sols">
      <xsl:apply-templates select="sol[attribute::nom=$nomsol]"/>
   </xsl:template>

   <xsl:template match="sol">
      <xsl:text>     1   </xsl:text><xsl:value-of select="@nom"/><xsl:text> </xsl:text>
      <xsl:apply-templates select="param"/><xsl:text>
</xsl:text>
      <xsl:text>     1   </xsl:text><xsl:apply-templates select="option"/><xsl:text>
</xsl:text>
      <xsl:text>     1   </xsl:text><xsl:apply-templates select="option//choix"/>
      <xsl:apply-templates select="tableau_entete"/>
      <xsl:apply-templates select="tableau"/>
   </xsl:template>

   <xsl:template match="param">
      <xsl:value-of select="."/><xsl:text> </xsl:text>
   </xsl:template>

   <xsl:template match="option">
      <xsl:value-of select="@choix"/><xsl:text> </xsl:text>
   </xsl:template>

   <xsl:template match="choix">
      <xsl:apply-templates select="param"/>
   </xsl:template>

   <xsl:template match="tableau_entete"></xsl:template>

   <xsl:template match="tableau">
    <xsl:text>
</xsl:text><xsl:text>     1   </xsl:text><xsl:apply-templates select="colonne" />
   </xsl:template>

   <xsl:template match="colonne">
      <xsl:value-of select="."/><xsl:text> </xsl:text>
  </xsl:template>

  </xsl:stylesheet>
