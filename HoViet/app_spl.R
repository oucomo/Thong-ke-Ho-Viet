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
library(magrittr)
library(highcharter)
library(shinyjs)
library(tmap)
library(tmaptools)
library(leaflet)
library(mapdeck)
library(sf)
library(data.table)

# devtools::install_github("yonicd/covrpage", dependencies = T)
library(covrpage)
library(rhub)
library(whoami)

# devtools::install_version("MASS", "7.3-51.1")

key <- ''    ## put your own token here
mapdeck(token = key)

dat <- readRDS("dat_spl.rds")
# dat <- sf::st_simplify(dat)
# dat_spl <- rmapshaper::ms_simplify(dat, keep = 0.02)

dat2 = as.data.table(dat)
dict = unique(dat2, by = c('NAME_1', 'NAME_2'))

# dict1 <- dat %>%
#   group_by(NAME_1) %>%
#   distinct(NAME_2) %>%
#   select(NAME_1, NAME_2)

# dat <- mai_df %>%
#   st_drop_geometry()
# dat$latitude <- jitter(dat$latitude)
# dat$longitude <- jitter(dat$longitude)

m_Ho <- c("Select All", as.character(sort(unique(dat$Ho))))
m_Tinh <- c("Select All", as.character(sort(unique(dat$NAME_1))))
m_Huyen <- c("Select All", as.character(sort(unique(dat$NAME_2))))

s2_Tinh <- as.character(sort(unique(dat$NAME_1)))
s2_Huyen <- as.character(sort(unique(dat$NAME_2)))
s2_Ho <- as.character(sort(unique(dat$Ho)))

# cleantable <- dat %>%
#   select(
#     Tinh = NAME_1,
#     Huyen = NAME_2,
#     Xa = NAME_3,
#     Songuoi = songuoi,
#     Danso = danso,
#     Propop = pro.pop,
#     DientichM = area,
#     DientichKM = area_km,
#     SonguoiKM = songuoi_km,
#     PropopKM = pro.pop_km,
#     Ho = Ho,
#     PropHo = prop.ho,
#     Lat = latitude,
#     Long = longitude
#   )

# Define UI for application
ui <- shinydashboard::dashboardPage(skin='black',
                                    shinydashboard::dashboardHeader(title = "Họ Việt",
                                                                    tags$li(a(href = 'https://duhongduc.shinyapps.io/HoViet/',
                                                                              img(src = 'vietnam.png', icon = icon("star"),
                                                                                  title = "Vietnam", height='30',width='100'),
                                                                              style = "padding-top:10px; padding-bottom:10px;"),
                                                                            class = "dropdown")),
                                    shinydashboard::dashboardSidebar(width=275,
                                                                     
                                                                     # The dynamically-generated user panel
                                                                     uiOutput("userpanel"),
                                                                     
                                                                     # Side Bar Menu
                                                                     sidebarMenu(style = "position: Scroll; overflow: visible;",id = "sidebarmenu",
                                                                                 
                                                                                 # menuItem("Overview", tabName = "iaa", icon = icon("globe")),
                                                                                 
                                                                                 menuItem("VN Dashboard", tabName = "cso", icon = icon("desktop"),
                                                                                          badgeLabel = "new",
                                                                                          badgeColor = "green"),
                                                                                 
                                                                                 conditionalPanel(
                                                                                   "input.sidebarmenu === 'cso'",
                                                                                   # a. FILTERS
                                                                                   useShinyjs(),
                                                                                   div(id = "form",
                                                                                       tags$hr(),
                                                                                       selectInput("i2_ho", "Ho", choices = m_Ho, bookmarkButton(id = "bookmark1")),
                                                                                       selectInput("i2_tinh", "Tinh", choices = m_Tinh, bookmarkButton(id = "bookmark2")),
                                                                                       selectInput("i2_huyen", "Huyen",choices = "", bookmarkButton(id = "bookmark3")),
                                                                                       column(6,offset = 6,height = 100,style='padding100px;',
                                                                                              actionButton("reset_button", "Reset",icon = icon("repeat")))
                                                                                   ))
                                                                     )
                                    ),
                                    dashboardBody(
                                      
                                      tabItems(
                                        # tabItem(tabName = "iaa",
                                        #         fluidRow(column(10, offset = 0.5,h1("MAPDECK"))),
                                        #         br(),
                                        #         fluidRow(column(10, offset = 2.5,mapdeckOutput('map_value', width = 1400, height = 800))),
                                        #         br()
                                        # ),
                                        tabItem(tabName = "cso",
                                                fluidRow(column(10, offset = 0.5, h1("VN DASHBOARD"))),
                                                fluidRow(style="height:50px;",
                                                         valueBoxOutput("count1",width = 2),
                                                         valueBoxOutput("count2",width = 2),
                                                         valueBoxOutput("count3",width = 2),
                                                         valueBoxOutput("count4",width = 2),
                                                         valueBoxOutput("count5",width = 2)
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
  setBookmarkExclude(c("bookmark1", "bookmark2", "bookmark3"))
  
  # Trigger bookmarking with either button
  observeEvent(input$bookmark1, {
    session$doBookmark()
  })
  observeEvent(input$bookmark2, {
    session$doBookmark()
  })
  observeEvent(input$bookmark3, {
    session$doBookmark()
  })
  
  outVar <- reactive({
    if (input$i2_tinh != "Select All"){
      current_tinh <- input$i2_tinh
      c("Select All", dict$NAME_2[dict$NAME_1==current_tinh])}
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
    dat %>%
      dplyr::filter(
        if (input$i2_ho == "Select All") {Ho %in% s2_Ho} else {Ho == input$i2_ho},
        if (input$i2_tinh == "Select All") {NAME_1 %in% s2_Tinh} else {NAME_1 == input$i2_tinh},
        if (input$i2_huyen == "Select All") {NAME_2 %in% s2_Huyen} else {NAME_2 == input$i2_huyen}
      )
  })
  
  # Value Box 1
  output$count1 <- renderValueBox({
    if (input$i2_ho == "Select All" & input$i2_tinh == "Select All" & input$i2_huyen == "Select All"){
      hc12 <- "-"
    }
    else{
      hc12 <- round(sum(as.numeric(filt_mai1()$songuoi)),digits = 0)
    }
    valueBox(paste0(hc12), "Số người", icon = icon("circle-user"),
             color = "green"
    )
  })
  
  # Value Box 2
  output$count2 <- renderValueBox({
    if (input$i2_ho == "Select All" & input$i2_tinh == "Select All" & input$i2_huyen == "Select All"){
      hc13 <- "-"
    }
    else{
      hc13 <-  round(sum(as.numeric(filt_mai1()$danso)),digits = 0)
    }
    valueBox( paste0(hc13), "Dân số", icon = icon("users"),
              color = "olive"
    )
  })
  
  
  # Value Box 3
  output$count3 <- renderValueBox({
    if (input$i2_ho == "Select All" & input$i2_tinh == "Select All" & input$i2_huyen == "Select All"){
      hc14 <- "-"
    }
    else{
      hc14 <- round(sum(as.numeric(filt_mai1()$area_km)),digits = 1)
    }
    valueBox(paste0(hc14), "Diện tích (km2)", icon = icon("users"),
             color = "olive"
    )
  })
  
  # Value Box 4
  output$count4 <- renderValueBox({
    if (input$i2_ho == "Select All" & input$i2_tinh == "Select All" & input$i2_huyen == "Select All"){
      hc15 <- "-"
    }
    else {
      hc15 <- round(sum(as.numeric(filt_mai1()$songuoi))/sum(as.numeric(filt_mai1()$area_km)),digits = 1)
    }
    valueBox(paste0(hc15), "Mật độ (người/km2)", icon = icon("circle-user"), color = "blue")
  })
  
  # Value Box 5
  output$count5 <- renderValueBox({
    if (input$i2_ho == "Select All" & input$i2_tinh == "Select All" & input$i2_huyen == "Select All"){
      hc16 <- "-"
    }
    else {
      hc16 <- round((sum(as.numeric(filt_mai1()$songuoi)) / sum(as.numeric(filt_mai1()$danso)))*100, digits = 2)
    }
    valueBox(paste0(hc16), "Tỉ lệ (%)", icon = icon("circle-user"), color = "yellow")
  })
  
  
  # Maps
  
  mappalette <- reactive ({
    colorNumeric("Reds", filt_mai1()$songuoi_km)
  })
  
  #Popup
  mappopup <- reactive ({
    paste(sep = "<br/>",
          "<b>Tỉnh: </b>",filt_mai1()$NAME_1,
          "<b>Huyện: </b>",filt_mai1()$NAME_2,
          "<b>Xã: </b>",filt_mai1()$NAME_3,
          "<i>Số người</i>",filt_mai1()$songuoi,
          "<i>Dân số</i>",filt_mai1()$danso,
          "<i>Diện tích (km2)</i>",round(as.numeric(filt_mai1()$area_km), digits = 1),
          "<i>Mật độ (người/km2)</i>",round(as.numeric(filt_mai1()$songuoi_km), digits = 1),
          "<i>Tỉ lệ (%)</i>", round(as.numeric(filt_mai1()$pro.pop), digits = 2))
  })
  
  output$map1 <- renderLeaflet({
    leaflet(filt_mai1()) %>%
      addProviderTiles(provider = "Esri.WorldStreetMap") %>%
      setView(lng = "108.2772", lat="16.0583", zoom = 6)
    
    # output$map1 <- renderLeaflet({
    #   leaflet(filt_mai1()) %>%
    #     addProviderTiles(providers$OpenStreetMap) %>%
    #     setView(lng = "108.2772", lat="16.0583", zoom = 6) %>%
    #     addProviderTiles("CartoDB.Positron")
    
  }) # render Leaflet
  
  observe({
    # pal1 <- mappalette()
    pal <- colorNumeric(palette = "viridis", reverse = TRUE, domain = filt_mai1()$pro.pop, alpha = TRUE)
    
    leafletProxy("map1", data = filt_mai1()) %>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.2,
                  fillOpacity = .75,
                  popup = mappopup(),
                  color = ~ pal(filt_mai1()$pro.pop)
      ) %>%
      addMiniMap(position = "bottomleft", width = 150, height = 150,
                 collapsedWidth = 19, collapsedHeight = 19, zoomLevelOffset = -5,
                 zoomLevelFixed = FALSE, centerFixed = FALSE, zoomAnimation = TRUE,
                 toggleDisplay = TRUE, autoToggleDisplay = TRUE, minimized = TRUE,
                 aimingRectOptions = list(color = "#ff7800", weight = 1, clickable = TRUE),
                 shadowRectOptions = list(color = "#000000", weight = 1, clickable = TRUE,
                                          opacity = 0, fillOpacity = 0), strings = list(hideText = "Hide MiniMap", showText = "Show MiniMap"),
                 tiles = (providers$OpenStreetMap), mapOptions = list()) %>%
      addLegend("bottomright", pal = pal, values = ~filt_mai1()$pro.pop,
                title = "Tỉ lệ (% người/dân số)",
                opacity = 1)
  })
  # 
  # output$map_value <- renderMapdeck({
  #   mapdeck(token = key,
  #           style = mapdeck_style("streets")
  #           ,pitch = 60
  #           ,zoom = 10
  #   ) %>%
  #     add_grid(
  #       data = dat
  #       , lat = "x"
  #       , lon = "y"
  #       , cell_size = 500
  #       , elevation_scale = 15
  #       , layer_id = "grid_layer"
  #     )
  # }) # render MapDeck
}

# Run the application
shinyApp(ui = ui, server = server)
