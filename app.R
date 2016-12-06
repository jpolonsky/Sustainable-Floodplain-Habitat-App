#
#
#
#
#



library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(plotly)

# import modules
source("global.R")
source("leafletGlobal.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Sustainable Floodplain Habitat Finder", 
                  titleWidth = 500),
  dashboardSidebar( 
                   sidebarMenu(
                     menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                     menuItem("About FlowWest", icon = icon("th"), 
                              tabName = "about",
                              badgeColor = "green")
                   )
  ), 
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(leafletOutput("mapView"),
              title = "Select a Monitoring Location", 
              width = 6, 
              status = "primary",
              solidHeader = TRUE, 
              collapsible = FALSE), 
          box(title = "Title 2", 
              width = 6,
              status = "primary",
              solidHeader = TRUE, 
              collapsible = FALSE)
        ), 
        fluidRow(
          box(title = "summary statistics", 
              width = 6, 
              solidHeader = TRUE, 
              status = "warning", 
              collapsible = FALSE),
          box(title = "summary statistics", 
              width = 6, 
              solidHeader = TRUE, 
              status = "warning", 
              collapsible = FALSE)
        )
      ), 
      tabItem(
        tabName = "about"
      )
    )
    
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  # Map view using leaflet  
  output$mapView <- renderLeaflet({
    leaflet(data = gwlLatLong) %>%
      
      # set a group of basemaps to choose from 
      addTiles(group = "Open Maps Street Map (default)") %>%
      addProviderTiles("CartoDB.Positron", group = "Light Map") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      
      # map layers to choose from
      
      # well data layer shows up by default 
      addCircleMarkers(clusterOptions = markerClusterOptions(),
                       lat = ~lat, 
                       lng = ~lng, 
                       stroke = TRUE, 
                       group = "Ground Water Wells") %>%
      
      # fish data can be added with a checkbox
      addMarkers(data = screwtrapLatLong, 
                 lng=~lng, 
                 lat=~lat,
                 group = "Screw Trap", 
                 layerId = "screwTrap_markers", 
                 icon = blueFishIcon) %>%
      
      # flow data marker
      addMarkers(data=flowLatLong, 
                 lat = ~lat, 
                 lng = ~lng, 
                 layerId = "flow_markers") %>%
      
      addLayersControl(
        baseGroups = c("Open Maps Street Map (default)", "Light Map", "Satellite"),
        overlayGroups = c("Ground Water Wells", "Screw Trap"), 
        options = layersControlOptions(collapsed = FALSE))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

