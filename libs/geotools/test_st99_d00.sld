<?xml version="1.0" encoding="ISO-8859-1"?>
<StyledLayerDescriptor version="1.0.0" xmlns="http://www.opengis.net/sld"
  xmlns:sld="http://www.opengis.net/sld" xmlns:ogc="http://www.opengis.net/ogc"
  xmlns:xlink="http://www.w3.org/1999/xlink"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="http://www.opengis.net/sld http://schemas.cubewerx.com/schemas/sld/1.0.0-cw/StyledLayerDescriptor.xsd">
  <NamedLayer>
    <Name>st99_d00</Name>
    <UserStyle>
      <Name>States</Name>
      <IsDefault>1</IsDefault>
      <FeatureTypeStyle>
        <FeatureTypeName>st99_d00</FeatureTypeName>
        <Rule>
          <PolygonSymbolizer>
            <Stroke>
              <CssParameter name="stroke">#000000</CssParameter>
              <CssParameter name="stroke-width">1</CssParameter>
            </Stroke>
						<Fill>
							<CssParameter name="fill">#FFFFFF</CssParameter>
						</Fill>
          </PolygonSymbolizer>
        </Rule>
        <!--
				<Rule>
					<TextSymbolizer>
						<Label>
							<ogc:PropertyName>STATE</ogc:PropertyName>
						</Label>
						<Font>
							<CssParameter name="font-style">normal</CssParameter>
						</Font>
					</TextSymbolizer>
				</Rule>
				-->
      </FeatureTypeStyle>
    </UserStyle>
  </NamedLayer>
</StyledLayerDescriptor>
