<?xml version="1.0"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="text" />

  <xsl:template match="/">
    <xsl:copy-of select="/html/head/title/node()"/>
    <xsl:text>&#xa;</xsl:text>
    <xsl:apply-templates select="/html/head/style"/>
    <xsl:text>&#xa;</xsl:text>
    <xsl:apply-templates select="/html/body/div/div[@class='test']"/>
    <xsl:text>&#xa;</xsl:text>
    <xsl:apply-templates select="/html/body/div/div[@class='ref']"/>
    <xsl:text>&#xa;</xsl:text>
  </xsl:template>

  <xsl:template match="/html/head/style">
    <xsl:variable name="styletext" select="text()"/>

    <xsl:choose>
      <xsl:when test="contains($styletext, 'lowercase')">
        <xsl:text>lowercase</xsl:text>
      </xsl:when>
      <xsl:when test="contains($styletext, 'uppercase')">
        <xsl:text>uppercase</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:message terminate='yes'>Case conversion is not found</xsl:message>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="/html/body/div/div">
    <xsl:for-each select="span">
      <xsl:value-of select="text()"/>
      <xsl:if test="position() != last()">
        <xsl:text> </xsl:text>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>
