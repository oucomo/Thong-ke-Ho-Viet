shinyUI(
  # Use a fluid Bootstrap layout
  fluidPage(
    # Give the page a title
    titlePanel("Mai"),
    mainPanel(leafletOutput("map"))
  )
)
