<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE stylesheet [
<!ENTITY space "<xsl:text> </xsl:text>">
<!ENTITY cr "<xsl:text>
</xsl:text>">
<!ENTITY numsol "<xsl:text>     1   </xsl:text>">
]>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
   
   <xsl:import href="nomsol.tmp.xsl" />
   
   <xsl:output method="text"/>
   
   <xsl:template match="/sols">
      <xsl:apply-templates select="sol[attribute::nom=$nomsol]"/>
   </xsl:template>
   
   <xsl:template match="sol">
      &numsol;<xsl:value-of select="@nom"/>&space;
      <xsl:apply-templates select="param"/>&cr;
      &numsol;<xsl:apply-templates select="option"/>&cr;
      &numsol;<xsl:apply-templates select="option//choix"/>
      <xsl:apply-templates select="tableau_entete"/>
      <xsl:apply-templates select="tableau"/>
   </xsl:template>
   
   <xsl:template match="param">
      <xsl:value-of select="."/>&space;
   </xsl:template>
   
   <xsl:template match="option">
      <xsl:value-of select="@choix"/>&space;
   </xsl:template>
   
   <xsl:template match="choix">
      <xsl:apply-templates select="param"/>
   </xsl:template>
   
   <xsl:template match="tableau_entete"></xsl:template>
   
   <xsl:template match="tableau">
    &cr;&numsol;<xsl:apply-templates select="colonne" />
   </xsl:template>

   <xsl:template match="colonne">
      <xsl:value-of select="."/>&space;
  </xsl:template>

  </xsl:stylesheet>
