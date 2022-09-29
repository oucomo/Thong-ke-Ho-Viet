shinyServer(function (input, output) {

  # Create our colors with a categorical color function
  pal <- colorQuantile(palette = "viridis", domain = mai$songuoi_km, n = 10)

  output$map <- renderLeaflet({
    leaflet(width = "100%") %>%
      addProviderTiles(provider = "Esri.WorldStreetMap") %>%
      addPolygons(data=mai,
                  popup = ~ str_extract(NAME_3, "^([^,]*)"),
                  stroke = FALSE,
                  smoothFactor = 0,
                  fillOpacity = 0.4,
                  color = ~ pal(songuoi_km)) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~ mai$songuoi_km,
                title = "Mật độ người/km2",
                opacity = 1)
  })
})
