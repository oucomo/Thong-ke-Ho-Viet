#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(png)
library(shinydashboard)
library(shinydashboardPlus)
library(shiny)
library(dplyr)
library(tidyverse)
library(magrittr)
library(highcharter)
library(shinyjs)
library(tmap)
library(tmaptools)
library(leaflet)
library(mapdeck)
library(sf)
library(data.table)

key <- ''    ## put your own token here
mapdeck(token = key)

mai <- readRDS("mai.rds")

mai$latitude <- jitter(mai$x)
mai$longitude <- jitter(mai$y)

m_Tinh <- c("Select All", as.character(sort(unique(mai$NAME_1))))
m_Huyen <- c("Select All", as.character(sort(unique(mai$NAME_2))))

dat2 = as.data.table(mai)
dict = unique(dat2[,.(NAME_1, NAME_2), by=NAME_1], by = 'NAME_1')

s2_Tinh <- as.character(sort(unique(mai$NAME_1)))
s2_Huyen <- as.character(sort(unique(mai$NAME_2)))

cleantable <- mai %>%
  select(
    Tinh = NAME_1,
    Huyen = NAME_2,
    Xa = NAME_3,
    Songuoi = songuoi,
    Danso = danso,
    Propop = pro.pop,
    DientichM = area,
    DientichKM = area_km,
    SonguoiKM = songuoi_km,
    PropopKM = pro.pop_km,
    Ho = Ho,
    PropHo = prop.ho,
    Lat = latitude,
    Long = longitude
  )

# Define UI for application
ui <- shinydashboard::dashboardPage(skin='black',
                                    shinydashboard::dashboardHeader(title = "Họ Việt"),
                                    shinydashboard::dashboardSidebar(width=275,
                                                                     
                                                                     # The dynamically-generated user panel
                                                                     uiOutput("userpanel"),
                                                                     
                                                                     # Side Bar Menu
                                                                     sidebarMenu(style = "position: Scroll; overflow: visible;",id = "sidebarmenu",
                                                                                 
                                                                                 menuItem("Overview", tabName = "iaa", icon = icon("th")),
                                                                                 
                                                                                 menuItem("VN Dashboard", tabName = "cso", icon = icon("desktop"),
                                                                                          badgeLabel = "new",
                                                                                          badgeColor = "green"),
                                                                                 
                                                                                 conditionalPanel(
                                                                                   "input.sidebarmenu === 'cso'",
                                                                                   # a. FILTERS
                                                                                   useShinyjs(),
                                                                                   div(id = "form",
                                                                                       tags$hr(),
                                                                                       selectInput("i2_tinh", "Tinh", choices = m_Tinh,bookmarkButton(id = "bookmark1")),
                                                                                       selectInput("i2_huyen", "Huyen", choices = m_Huyen,bookmarkButton(id = "bookmark2")),
                                                                                       column(6,offset = 6,height = 100,style='padding100px;',
                                                                                              actionButton("reset_button", "Reset",icon = icon("repeat")))
                                                                                   ))
                                                                     )
                                    ),
                                    dashboardBody(
                                      
                                      tabItems(
                                        tabItem(tabName = "iaa",
                                                fluidRow(column(10, offset = 0.5,h1("MAPDECK"))),
                                                br(),
                                                fluidRow(column(10, offset = 2.5,mapdeckOutput('map_value', width = 1400, height = 800))),
                                                br()
                                        ),
                                        tabItem(tabName = "cso",
                                                fluidRow(column(10, offset = 0.5, h1("VN DASHBOARD"))),
                                                fluidRow(style="height:50px;",
                                                         valueBoxOutput("count1",width = 3),
                                                         valueBoxOutput("count2",width = 3),
                                                         valueBoxOutput("count3",width = 3),
                                                         valueBoxOutput("count4",width = 3)
                                                ),
                                                br(),
                                                br(),
                                                fluidRow(column(10, offset = 2.5,leafletOutput('map1', width = 1400, height = 800)))
                                        )
                                      )
                                    )
)

# Define server logic required
server <- function(input, output, session) {
  
  addClass(selector = "body", class = "sidebar-collapse")
  
  # Reset Button
  
  # Need to exclude the buttons from themselves being bookmarked
  setBookmarkExclude(c("bookmark1", "bookmark2"))
  
  # Trigger bookmarking with either button
  observeEvent(input$bookmark1, {
    session$doBookmark()
  })
  observeEvent(input$bookmark2, {
    session$doBookmark()
  })
  
  outVar <- reactive({
    if (input$i2_tinh != "Select All"){
      current_tinh <- input$i2_tinh
      dict$NAME_2[dict$NAME_1==current_tinh]} 
    else {
      m_Huyen
    }
  })
  
  observe({
    updateSelectInput(session, "i2_huyen",choices = outVar())
  })
  
  js_click_line <- JS("function(event) {Shiny.onInputChange('line_clicked', [event.point.category]);}")
  
  observeEvent(input$reset_button, {
    reset("form")
  })
  
  id <- NULL
  
  observeEvent(input$reset_button, {
    
    id <<- showNotification(
      paste("Filters are Reset"),
      duration = 5,
      type = "message"
    )
  })
  
  # Data
  
  filt_mai1 <- reactive({
    mai %>%
      filter(
        if (input$i2_tinh == "Select All") {NAME_1 %in% s2_Tinh} else {NAME_1 == input$i2_tinh},
        if (input$i2_huyen == "Select All") {NAME_2 %in% s2_Huyen} else {NAME_2 == input$i2_huyen}
      )
  })
  
  # Value Box 1
  output$count1 <- renderValueBox({
    hc12 <- round(sum(as.numeric(filt_mai1()$songuoi)),digits = 0)
    valueBox(paste0(hc12), "Số người", icon = icon("users"),
             color = "green"
    )
  })
  
  # Value Box 2
  output$count2 <- renderValueBox({
    hc13 <-  round(sum(as.numeric(filt_mai1()$danso)),digits = 0)
    valueBox( paste0(hc13), "Dân số", icon = icon("users"),
              color = "olive"
    )
  })
  
  
  # Value Box 3
  output$count3 <- renderValueBox({
    hc14 <- round(sum(as.numeric(filt_mai1()$area_km)),digits = 2)
    valueBox(paste0(hc14), "Diện tích (km2)", icon = icon("circle-user"),
             color = "blue"
    )
  })
  
  # Value Box 4
  output$count4 <- renderValueBox({
    hc15 <- round(sum(as.numeric(filt_mai1()$pro.pop)),digits = 2)
    valueBox(paste0(hc15), "Tỉ lệ", icon = icon("circle-user"),
             color = "blue"
    )
  })
  
  # Maps

  mappalette <- reactive ({
    colorNumeric("Reds", filt_mai1()$songuoi_km)
  })

  #Popup
  mappopup <- reactive ({
    paste(sep = "<br/>",
          "<b>Huyen: </b>",filt_mai1()$NAME_2,
          "<i>Tong so nguoi</i>",filt_mai1()$songuoi,
          "<i>Dien tich</i>",filt_mai1()$area,
          "<i>Ti le</i>", filt_mai1()$pro.pop)
  })

  output$map1 <- renderLeaflet({
    leaflet(filt_mai1()) %>%
      addProviderTiles(provider = "Esri.WorldStreetMap") %>%
      setView(lng = "108.2772", lat="16.0583", zoom = 6)

  }) # render Leaflet

  observe({
    # pal1 <- mappalette()
    pal <- colorNumeric(palette = "viridis", reverse = TRUE, domain = mai$songuoi_km, alpha = TRUE)

    leafletProxy("map1", data = filt_mai1()) %>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0,
                  fillOpacity = .4,
                  popup = mappopup(),
                  color = ~ pal(mai$songuoi_km)
      ) %>%
      addMiniMap(position = "bottomleft", width = 150, height = 150,
                 collapsedWidth = 19, collapsedHeight = 19, zoomLevelOffset = -5,
                 zoomLevelFixed = FALSE, centerFixed = FALSE, zoomAnimation = TRUE,
                 toggleDisplay = TRUE, autoToggleDisplay = TRUE, minimized = TRUE,
                 aimingRectOptions = list(color = "#ff7800", weight = 1, clickable = TRUE),
                 shadowRectOptions = list(color = "#000000", weight = 1, clickable = TRUE,
                                          opacity = 0, fillOpacity = 0), strings = list(hideText = "Hide MiniMap", showText = "Show MiniMap"),
                 tiles = (providers$OpenStreetMap), mapOptions = list()) %>%
      addLegend("bottomright", pal = pal, values = ~filt_mai1()$songuoi_km,
                title = "Mật độ người/km2",
                opacity = 1)
  })

  output$map_value <- renderMapdeck({
    mapdeck(token = key,
            style = mapdeck_style('satellite-streets')
            ,pitch = 60
            ,zoom = 10
    ) %>%
      add_grid(
        data = cleantable
        , lat = "Lat"
        , lon = "Long"
        , cell_size = 500
        , elevation_scale = 15
        , layer_id = "grid_layer"
      )
  }) # render MapDeck
}

# Run the application
shinyApp(ui = ui, server = server)
