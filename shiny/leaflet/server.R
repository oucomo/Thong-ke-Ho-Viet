library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
#zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
maidata <- mai[order(mai$centile),]

function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 108.2772, lat = 16.0583, zoom = 4)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  maiInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(maidata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(maidata,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })

  # Precalculate the breaks we'll need for the two histograms
  songuoiBreaks <- hist(plot = FALSE, mai$songuoi, breaks = 20)$breaks

  output$histSonguoi <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(maiInBounds()) == 0)
      return(NULL)

    hist(maiInBounds()$songuoi,
         breaks = songuoiBreaks,
         main = "So nguoi (mai)",
         xlab = "Percentile",
         xlim = range(mai$songuoi),
         col = '#00DD00',
         border = 'white')
  })

  output$scatterSonguoiKM <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(maiInBounds()) == 0)
      return(NULL)

    print(xyplot(songuoi_km ~ NAME_3, data = maiInBounds(), xlim = range(mai$NAME_3), ylim = range(mai$songuoi_km)))
  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size

    if (colorBy == "superzip") {
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
      colorData <- ifelse(maidata$songuoi >= (100 - input$threshold), "yes", "no")
      pal <- colorFactor("viridis", colorData)
    } else {
      colorData <- maidata[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    }

    if (sizeBy == "superzip") {
      # Radius is treated specially in the "superzip" case.
      radius <- ifelse(maidata$songuoi >= (100 - input$threshold), 30000, 3000)
    } else {
      radius <- maidata[[sizeBy]] / max(maidata[[sizeBy]]) * 30000
    }

    leafletProxy("map", data = maidata) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~songuoi_km,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })

  # Show a popup at the given location
  showSonguoiKMPopup <- function(songuoi_km, lat, lng) {
    selectedSonguoiKM <- mai[mai$songuoi_km == songuoi_km,]
    content <- as.character(tagList(
      tags$h4("So nguoi/KM2:", as.integer(selectedSonguoiKM$songuoi_km)),
      tags$strong(HTML(sprintf("%s, %s %s",
                               selectedSonguoiKM$tinh, selectedSonguoiKM$huyen, selectedSonguoiKM$xa
      ))), tags$br(),
      sprintf("Median household income: %s", dollar(selectedSonguoiKM$songuoi_km * 1000)), tags$br(),
      sprintf("Percent of adults with BA: %s%%", as.integer(selectedSonguoiKM$songuoi_km)), tags$br(),
      sprintf("Adult population: %s", selectedSonguoiKM$danso)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showSonguoiKMPopup(event$id, event$lat, event$lng)
    })
  })


  ## Data Explorer ###########################################

  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectizeInput(session, "cities", choices = cities,
                         selected = stillSelected, server = TRUE)
  })

  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
               is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectizeInput(session, "zipcodes", choices = zipcodes,
                         selected = stillSelected, server = TRUE)
  })

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })

  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Score >= input$minScore,
        Score <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df, outputId = "ziptable")

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
