<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  version="1.0">
  <xsl:output method="xml"/>

  <xsl:template match="/">
    <fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format">
      <fo:layout-master-set>                  
	<fo:simple-page-master  
	   master-name="simple"                 
           page-height  ="29.7cm"   
           page-width   ="21cm"
           margin-left  ="2.5cm"
	   margin-right ="2.5cm">

	  <fo:region-body
	     region-name="Content"
	     margin-top="1.0cm" margin-bottom="1in"
	     />
    
	  <fo:region-before extent=".8cm"/>
	  <fo:region-after extent=".5in" background-color="silver"/>
    	</fo:simple-page-master>
      </fo:layout-master-set> 
      <fo:page-sequence 
	 master-reference="simple">  
	
	<fo:static-content flow-name="xsl-region-before">
	  <fo:block>
	    <fo:inline keep-together.within-line="always">
              <fo:leader leader-pattern="rule" 
			 leader-length="2.95cm"/>
	      <fo:inline vertical-align="sub">
		Chapter NNNNN
	      </fo:inline>
	      <fo:leader leader-pattern="rule"
			 leader-length="2.95cm" /></fo:inline>
	  </fo:block>
	</fo:static-content>
	
	<fo:flow flow-name="Content">  
	  <xsl:apply-templates/>              
	</fo:flow>
      </fo:page-sequence>
    </fo:root>
  </xsl:template>
  
  <xsl:template match="chap">          
    <xsl:apply-templates/>
  </xsl:template>
  
  <xsl:template match="h2">        
    <fo:block  font-size="14pt"
	       font-weight="bold"
	       space-before="1em"
               space-after="0.5em"	
               space-after.conditionality = 'retain'
	       >
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>

  <!-- this is my h1 -->
  
  <xsl:template match="title">        
    <fo:block  
       margin-top="4mm"
       margin-bottom="4mm"
       padding-before="0.4em"
       padding-after="0.4em"
       text-align="center"
       background-color="black" 
       color="white" 
       font-family="Times" 
       font-size="30pt"
       span = "all"
       font-weight="bold"
       space-after="1cm">
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>

  <xsl:template match="strike">
    <fo:inline text-decoration="line-through">
      <xsl:apply-templates select="*|text()"/>
    </fo:inline>
  </xsl:template>
  
  <xsl:template match="note"> 
    <fo:block border-style="solid"
	      margin="0.2cm"
	      border-width="1px"
	      padding="4mm"
	      text-align="justify"  
	      font-family="Helvetica"> 
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  
  
  <xsl:template match="warn"> 
    <fo:block margin="0px" padding="8px" background-color="#aabbcc"> 
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  
  <xsl:template match="box"> 
    <fo:block border-style="solid"
	      margin="0.2cm"
	      border-width="5px"
	      border-color="red"
	      padding="4mm"
	      text-align="justify"  
	      font-family="Helvetica"> 
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  
  <xsl:template match="p">
    <fo:block text-align="justify"  font-family="Times"> 
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>


  <!-- ============================================
    We handle an ordered list with two complications:
    If the list appears inside another list (either 
    an <ol> or <ul>), we don't put any vertical space
    after it.  The other issue is that we indent the
    list according to how deeply nested the list is. 
    =============================================== -->

  <xsl:template match="ol">
    <fo:list-block provisional-distance-between-starts="1cm"
      provisional-label-separation="0.5cm">
      <xsl:attribute name="space-after">
        <xsl:choose>
          <xsl:when test="ancestor::ul or ancestor::ol">
            <xsl:text>0pt</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>12pt</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
      <xsl:attribute name="start-indent">
        <xsl:variable name="ancestors">
          <xsl:choose>
            <xsl:when test="count(ancestor::ol) or count(ancestor::ul)">
              <xsl:value-of select="1 + 
                                    (count(ancestor::ol) + 
                                     count(ancestor::ul)) * 
                                    1.25"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>1</xsl:text>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:value-of select="concat($ancestors, 'cm')"/>
      </xsl:attribute>
      <xsl:apply-templates select="*"/>
    </fo:list-block>
  </xsl:template>

  <!-- ============================================
    When we handle items in an ordered list, we need
    to check if the list has a start attribute.  If
    it does, we change the starting number accordingly.
    Once we've figured out where to start counting,
    we check the type attribute to see what format
    the numbers should use.  
    =============================================== -->

  <xsl:template match="ol/li">
    <fo:list-item>
      <fo:list-item-label end-indent="label-end()">
        <fo:block>
          <xsl:variable name="value-attr">
            <xsl:choose>
              <xsl:when test="../@start">
                <xsl:number value="position() + ../@start - 1"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:number value="position()"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:variable>
          <xsl:choose>
            <xsl:when test="../@type='i'">
              <xsl:number value="$value-attr" format="i. "/>
            </xsl:when>
            <xsl:when test="../@type='I'">
              <xsl:number value="$value-attr" format="I. "/>
            </xsl:when>
            <xsl:when test="../@type='a'">
              <xsl:number value="$value-attr" format="a. "/>
            </xsl:when>
            <xsl:when test="../@type='A'">
              <xsl:number value="$value-attr" format="A. "/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:number value="$value-attr" format="1. "/>
            </xsl:otherwise>
          </xsl:choose>
        </fo:block>
      </fo:list-item-label>
      <fo:list-item-body start-indent="body-start()">
        <fo:block>
          <xsl:apply-templates select="*|text()"/>
        </fo:block>
      </fo:list-item-body>
    </fo:list-item>
  </xsl:template>

  <!-- ============================================
    The unordered list is pretty straightforward; 
    the only complication is calculating the space-
    after and start-indent properties.  If this 
    list is inside another list, we don't put any 
    space after this one, and we calculate the 
    indentation based on the nesting level of this 
    list. 
    =============================================== -->

  <xsl:template match="ul">
    <fo:list-block provisional-distance-between-starts="1cm"
      provisional-label-separation="0.5cm">
      <xsl:attribute name="space-after">
        <xsl:choose>
          <xsl:when test="ancestor::ul or ancestor::ol">
            <xsl:text>0pt</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>12pt</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
      <xsl:attribute name="start-indent">
        <xsl:variable name="ancestors">
          <xsl:choose>
            <xsl:when test="count(ancestor::ol) or count(ancestor::ul)">
              <xsl:value-of select="1 + 
                                    (count(ancestor::ol) + 
                                     count(ancestor::ul)) * 
                                    1.25"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>1</xsl:text>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:value-of select="concat($ancestors, 'cm')"/>
      </xsl:attribute>
      <xsl:apply-templates select="*"/>
    </fo:list-block>
  </xsl:template>


  <!-- ============================================
    Preformatted text is rendered in a monospaced
    font.  We also have to set the wrap-option
    and white-space-collapse properties.  
    =============================================== -->

  <xsl:template match="pre">
    <fo:block font-family="monospace"
	      linefeed-treatment="preserve"
	      white-space-collapse="false" 
	      white-space-treatment="preserve" 
	      >
      <xsl:apply-templates select="*|text()"/>
    </fo:block>
  </xsl:template>

  <!-- ============================================
    We don't do anything with the <dl> element, we
    just handle the elements it contains.  Notice
    that we're ignoring any text that appears 
    in the <dl> itself; I'm not sure if that's
    the right call.
    =============================================== -->

  <xsl:template match="dl">
    <xsl:apply-templates select="*"/>
  </xsl:template>

  <!-- ============================================
    A definition term is rendered in bold.  We 
    specify keep-with-next here, although it doesn't
    always work with FOP.
    =============================================== -->

  <xsl:template match="dt">
    <fo:block font-weight="bold" space-after="2pt"
      keep-with-next="always">
      <xsl:apply-templates select="*|text()"/>
    </fo:block>
  </xsl:template>

  <!-- ============================================
    We handle each <dd> element as an indented block
    beneath the defined term.  If the following 
    element is another <dd>, that means it's another
    definition for the same term.  In that case, 
    we don't put as much vertical space after the 
    block. 
    =============================================== -->

  <xsl:template match="dd">
    <fo:block start-indent="1cm">
      <xsl:attribute name="space-after">
        <xsl:choose>
          <xsl:when test="name(following::*[1]) = 'dd'">
            <xsl:text>3pt</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>12pt</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
      <xsl:apply-templates select="*|text()"/>
    </fo:block>
  </xsl:template>

  <!-- ============================================
    List items inside unordered lists are easy; we
    just have to use the correct Unicode character
    for the bullet.  
    =============================================== -->

  <xsl:template match="ul/li">
    <fo:list-item>
      <fo:list-item-label end-indent="label-end()">
        <fo:block>&#x2022;</fo:block>
      </fo:list-item-label>
      <fo:list-item-body start-indent="body-start()">
        <fo:block>
          <xsl:apply-templates select="*|text()"/>
        </fo:block>
      </fo:list-item-body>
    </fo:list-item>
  </xsl:template>


  <!-- ============================================
    Teletype text is rendered in a monospaced font.
    =============================================== -->

  <xsl:template match="c">
    <fo:inline font-family="monospace">
      <xsl:apply-templates select="*|text()"/>
    </fo:inline>
  </xsl:template>
 

  <!-- ============================================
    For bold elements, we just change the font-weight.
    =============================================== -->

  <xsl:template match="b">
    <fo:inline font-weight="bold">
      <xsl:apply-templates select="*|text()"/>
    </fo:inline>
  </xsl:template>


 <!-- ============================================
    Italics.  You can't get much simpler than that.
    =============================================== -->

  <xsl:template match="i">
    <fo:inline font-style="italic">
      <xsl:apply-templates select="*|text()"/>
    </fo:inline>
  </xsl:template>
  
  <xsl:template match="by">
    <fo:inline
       padding='.3mm'
       font-style="bold"
       background-color="orange">
      <xsl:apply-templates select="*|text()"/>
    </fo:inline>
  </xsl:template>

 
  <!-- ============================================
    For underlined text, we use the text-decoration
    property.
    =============================================== -->

  <xsl:template match="u">
    <fo:inline text-decoration="underline">
      <xsl:apply-templates select="*|text()"/>
    </fo:inline>
  </xsl:template>


    
  <!-- ============================================
    For the <img> element, we use the src attribute
    as it comes from HTML.  We also check for any 
    width and height attributes.  If those attributes
    are there, we try to use them; height="300px" is
    used as-is, while height="300" is converted to 
    the value "300px".
    =============================================== -->

  <xsl:template match="img">
    <fo:block  text-align="center" space-after="12pt">
      <fo:external-graphic src="{@src}">
        <xsl:if test="@width">
          <xsl:attribute name="content-width">
            <xsl:choose>
              <xsl:when test="contains(@width, 'px')">
                <xsl:value-of select="@width"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="concat(@width, 'px')"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:attribute>
        </xsl:if>
        <xsl:if test="@height">
          <xsl:attribute name="content-height">
            <xsl:choose>
              <xsl:when test="contains(@height, 'px')">
                <xsl:value-of select="@height"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="concat(@height, 'px')"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:attribute>
        </xsl:if>
      </fo:external-graphic>
    </fo:block>
  </xsl:template>

  <xsl:template match="em">             
    <fo:inline font-style="italic">
      <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>
 
  <xsl:template match="*">          
    <fo:block background-color="red">
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>

</xsl:stylesheet>

