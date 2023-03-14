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
      <xsl:apply-templates select="hinit | NO3init | NH4init | Hinitf | NO3initf | NH4initf"/>
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
<!--for keeping compat with v9 -->
<xsl:apply-templates select="masec0 | QNplante0"/>

      <xsl:value-of select="magrain0"/><xsl:text>
</xsl:text>
      <xsl:value-of select="zrac0"/><xsl:text>
</xsl:text>

<!--for keeping compat with v9 -->
<xsl:apply-templates select="resperenne0"/>

<!-- for getting option content -->
   <xsl:apply-templates select="option"/>
      <xsl:apply-templates select="densinitial" /> 
   </xsl:template>
   
<xsl:template match="densinitial">
      <xsl:text>:densinitial:</xsl:text><xsl:text>
</xsl:text>
      <xsl:apply-templates select="horizon"/><xsl:text>
</xsl:text>
</xsl:template>
   
<!--xsl:template match="hinit"-->
<!-- contains: hinit or Hinit -->
<xsl:template match='*[(contains(name(), "Hinit")) or (contains(name(), "hinit"))]'>
      <!--xsl:text>:hinit:</xsl:text><xsl:text-->
      <xsl:text>:</xsl:text><xsl:value-of select="name()"/><xsl:text>:</xsl:text><xsl:text>
</xsl:text>
      <xsl:apply-templates select="horizon"/><xsl:text>
</xsl:text>
   </xsl:template>

<!--xsl:template match="NO3init"-->
<xsl:template match='*[contains(name(), "NO3init")]'>
      <!--xsl:text>:NO3init:</xsl:text><xsl:text-->
      <xsl:text>:</xsl:text><xsl:value-of select="name()"/><xsl:text>:</xsl:text><xsl:text>
</xsl:text>
      <xsl:apply-templates select="horizon"/><xsl:text>
</xsl:text>
</xsl:template>

<!--xsl:template match="NH4init"-->
<xsl:template match='*[contains(name(), "NH4init")]'>
      <!--xsl:text>:NH4init:</xsl:text><xsl:text-->
      <xsl:text>:</xsl:text><xsl:value-of select="name()"/><xsl:text>:</xsl:text><xsl:text>
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
      <xsl:text>Sdepth0</xsl:text><xsl:text>
</xsl:text>
      <xsl:value-of select="Sdepth0"/><xsl:text>
</xsl:text>
      <xsl:text>Sdry0</xsl:text><xsl:text>
</xsl:text>
      <xsl:value-of select="Sdry0"/><xsl:text>
</xsl:text>
      <xsl:text>Swet0</xsl:text><xsl:text>
</xsl:text>
      <xsl:value-of select="Swet0"/><xsl:text>
</xsl:text>
      <xsl:text>ps0</xsl:text><xsl:text>
</xsl:text>
      <xsl:value-of select="ps0"/><xsl:text>
</xsl:text>
</xsl:template>
   

<xsl:template match="option"><xsl:value-of select="@nomParam" /><xsl:text>
</xsl:text><xsl:value-of select="@choix" /><xsl:text>
</xsl:text>
<xsl:apply-templates select="choix"/>
</xsl:template>

<xsl:template match="choix">
<xsl:apply-templates select="maperenne0"/>

<xsl:apply-templates select="QNperenne0"/>

<xsl:apply-templates select="masecnp0"/>

<xsl:apply-templates select="QNplantenp0"/>

<xsl:apply-templates select="masec0"/>

<xsl:apply-templates select="QNplante0"/>

<xsl:apply-templates select="restemp0"/>

<!-- to be removed -->
<xsl:apply-templates select="resperenne0"/>

</xsl:template>

<xsl:template match="maperenne0">
      <xsl:value-of select="."/><xsl:text>
</xsl:text>
</xsl:template>

<xsl:template match="QNperenne0">
      <xsl:value-of select="."/><xsl:text>
</xsl:text>
</xsl:template>

<xsl:template match="masecnp0">
      <xsl:value-of select="."/><xsl:text>
</xsl:text>
</xsl:template>

<xsl:template match="QNplantenp0">
      <xsl:value-of select="."/><xsl:text>
</xsl:text>
</xsl:template>

<!-- working for both v9 and v10 -->
<xsl:template match="masec0">
      <xsl:value-of select="."/><xsl:text>
</xsl:text>
</xsl:template>

<xsl:template match="QNplante0">
      <xsl:value-of select="."/><xsl:text>
</xsl:text>
</xsl:template>

<xsl:template match="restemp0">
      <xsl:value-of select="."/><xsl:text>
</xsl:text>
</xsl:template>

<!-- to be removed -->
<xsl:template match="resperenne0">
      <xsl:value-of select="."/><xsl:text>
</xsl:text>
</xsl:template>



</xsl:stylesheet>
