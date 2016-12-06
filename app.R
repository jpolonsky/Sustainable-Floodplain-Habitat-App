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
ui <- dashboardPage(skin = "green",
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
                            infoBoxOutput("flowThreshold"),
                            infoBoxOutput("flowTodaysNeed"), 
                            infoBoxOutput("flowNaturalFlow")
                          )
                        ), 
                        tabItem(
                          tabName = "about", 
                          tags$h4("Some information about FLowWest, who are, what we do")
                        )
                      )
                      
                    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Leaflet Map View ===========================================================
  #
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
  
  # Infoboxes Render =================================================================
  # 
  output$flowThreshold <- renderInfoBox({
    infoBox(
      "The flow threshold is currently at", round(rnorm(1, mean=20000, sd=100)), "cfs", 
      color = "green", fill = TRUE
    )
  })
  output$flowTodaysNeed <- renderInfoBox({
    infoBox(
      "Today's need is at", round(rnorm(1, mean=100, sd=5)), "cfs", 
      color = "yellow", fill = TRUE
    )
  })
  output$flowNaturalFlow <- renderInfoBox({
    infoBox(
      "Natural Flow in", paste(sample(1:20, size = 1), "Days", sep = " "), 
      color = "orange", fill = TRUE
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

