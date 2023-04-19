<?xml version="1.0" encoding="UTF-8" ?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="text"/>

<xsl:template match="/">
<xsl:apply-templates select="//formalisme" />
</xsl:template>

<xsl:template match="formalisme">
<xsl:apply-templates select="param | option | tv | ta"/>
</xsl:template>

<xsl:template match="param"><xsl:value-of select="@nom" /><xsl:text>
</xsl:text><xsl:value-of select="." /><xsl:text>
</xsl:text>
</xsl:template>

<xsl:template match="option"><xsl:value-of select="@nomParam" /><xsl:text>
</xsl:text><xsl:value-of select="@choix" /><xsl:text>
</xsl:text>
<xsl:apply-templates select="choix"/>
</xsl:template>

<xsl:template match="choix">
<xsl:apply-templates select="param | option | ta"/>
</xsl:template>

<xsl:template match="tv">
<xsl:apply-templates select="variete"/>
</xsl:template>

<xsl:template match="variete">
<xsl:text>codevar
</xsl:text><xsl:value-of select="@nom" /><xsl:text>
</xsl:text>
<xsl:apply-templates select="param | optionv"/>
</xsl:template>

<xsl:template match="optionv">
<xsl:apply-templates select="param | optionv"/>
</xsl:template>

<xsl:template match="ta">
<xsl:text>nbinterventions
</xsl:text><xsl:value-of select="@nb_interventions" /><xsl:text>
</xsl:text>
<xsl:apply-templates select="ta_entete | intervention"/>
</xsl:template>

<xsl:template match="ta_entete"></xsl:template>

<xsl:template match="intervention">
<xsl:text>opp1
</xsl:text>
<xsl:apply-templates select="colonne"/>
<xsl:text>
</xsl:text>
</xsl:template>

<xsl:template match="colonne">
<xsl:value-of select="." /><xsl:text> </xsl:text>
</xsl:template>
</xsl:stylesheet>