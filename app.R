# Title: Sustainable Floodplain Habitat Dashboard
# Author: Emanuel Rodriguez
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
                    # Dashboard header ========================================================
                    dashboardHeader(title = "Sustainable Floodplain Habitat Finder", 
                                    titleWidth = 500),
                    
                    # Dashboard Sidebar =======================================================
                    dashboardSidebar( 
                      sidebarMenu(
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                        menuItem("About FlowWest", icon = icon("th"), 
                                 tabName = "about",
                                 badgeColor = "green"), 
                        menuItem("Data Sources", tabName = "rawData", icon = icon("list-alt")), 
                        menuItem("Source Code", icon = icon("github-alt"), 
                                 href = "https://github.com/ERGZ/Sustainable-Floodplain-Habitat-App")
                      )
                    ), 
                    
                    # Dashboard Body ========================================================
                    dashboardBody(
                      tabItems(
                        tabItem(
                          
                          # Main Dashboard
                          tabName = "dashboard",
                          fluidRow(
                            
                            # Map View Box
                            box(leafletOutput("mapView"),
                                title = "Select a Monitoring Location", 
                                width = 12, 
                                status = "primary",
                                solidHeader = TRUE, 
                                collapsible = FALSE)
                          ), 
                          fluidRow(
                            
                            # Visualizations Box
                            tabBox(width = 8,
                              side = "left",
                              title = "Visualizations", 
                              id = "visulizationTabs", 
                              tabPanel("Flow"), 
                              tabPanel("Ground Water"), 
                              tabPanel("Juvenile Salmon")
                            ),
                            
                            # Analytic Results 
                            column(width = 4,
                                   fluidRow(
                                     infoBoxOutput("flowThreshold", width = 12) 
                                   ),
                                   fluidRow(
                                     infoBoxOutput("flowTodaysNeed", width = 12)
                                   ),
                                   fluidRow(
                                     infoBoxOutput("flowNaturalFlow", width = 12)
                                   )
                                   
                            )
                            
                          )
                        ), 
                        tabItem(
                          tabName = "about",
                          includeHTML("flowwest_about.html")
                        ), 
                        tabItem(
                          tabName = "rawData", 
                          tags$h2("Let users look at and get raw data in this app")
                        )
                      )
                      
                    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Render Leaflet MapView ===========================================================
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
  
  # Render Infoboxes =================================================================
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
  
  # Render Visualizations ===========================================================
  #
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

