<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
   <xsl:output method="text"/>

   <xsl:template match="/initialisations">
      <xsl:apply-templates select="nbplantes"/>
      <xsl:apply-templates select="plante[attribute::dominance='1']"/>
      <xsl:apply-templates select="plante[attribute::dominance='2']"/>
      <xsl:apply-templates select="sol"/>
      <xsl:apply-templates select="snow"/>
   </xsl:template>
   
   <xsl:template match="sol">
      <xsl:apply-templates select="hinit | NO3init | NH4init"/>
   </xsl:template>
   
   <xsl:template match="nbplantes">
      <xsl:text>:nbplantes:</xsl:text><xsl:text>
</xsl:text>
      <xsl:value-of select="."/><xsl:text>
</xsl:text>
   </xsl:template>
   
   <xsl:template match="plante">
      <xsl:text>:plante:</xsl:text><xsl:text>
</xsl:text>
      <xsl:value-of select="stade0"/><xsl:text>
</xsl:text>
      <xsl:value-of select="lai0"/><xsl:text>
</xsl:text>
      <xsl:value-of select="masec0"/><xsl:text>
</xsl:text>
      <xsl:value-of select="QNplante0"/><xsl:text>
</xsl:text>
      <xsl:value-of select="magrain0"/><xsl:text>
</xsl:text>
      <xsl:value-of select="zrac0"/><xsl:text>
</xsl:text>
      <xsl:value-of select="resperenne0"/><xsl:text>
</xsl:text>
      <xsl:apply-templates select="densinitial" /> 
   </xsl:template>
   
   <xsl:template match="densinitial">
      <xsl:text>:densinitial:</xsl:text><xsl:text>
</xsl:text>
      <xsl:apply-templates select="horizon"/><xsl:text>
</xsl:text>
   </xsl:template>
   
   <xsl:template match="hinit">
      <xsl:text>:hinit:</xsl:text><xsl:text>
</xsl:text>
      <xsl:apply-templates select="horizon"/><xsl:text>
</xsl:text>
   </xsl:template>
   
   <xsl:template match="NO3init">
      <xsl:text>:NO3init:</xsl:text><xsl:text>
</xsl:text>
      <xsl:apply-templates select="horizon"/><xsl:text>
</xsl:text>
   </xsl:template>
   
   <xsl:template match="NH4init">
      <xsl:text>:NH4init:</xsl:text><xsl:text>
</xsl:text>
      <xsl:apply-templates select="horizon"/><xsl:text>
</xsl:text>
   </xsl:template>

   <xsl:template match="horizon">
      <xsl:value-of select="."/><xsl:text> </xsl:text>
   </xsl:template>
   
   <xsl:template match="snow">
      <xsl:text>:snow:</xsl:text><xsl:text>
</xsl:text>
      <xsl:text>SDepth</xsl:text><xsl:text>
</xsl:text>
      <xsl:value-of select="SDepth"/><xsl:text>
</xsl:text>
      <xsl:text>Sdry</xsl:text><xsl:text>
</xsl:text>
      <xsl:value-of select="Sdry"/><xsl:text>
</xsl:text>
      <xsl:text>Swet</xsl:text><xsl:text>
</xsl:text>
      <xsl:value-of select="Swet"/><xsl:text>
</xsl:text>
      <xsl:text>ps</xsl:text><xsl:text>
</xsl:text>
      <xsl:value-of select="ps"/><xsl:text>
</xsl:text>
   </xsl:template>
</xsl:stylesheet>
