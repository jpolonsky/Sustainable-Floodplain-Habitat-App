# Title: Sustainable Floodplain Habitat Dashboard
# Author: Emanuel Rodriguez
# Description:
#
#
# ================================================================================


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
                        menuItem("Data Sources", tabName = "rawData", icon = icon("list-alt")),
                        menuItem("About FlowWest", icon = icon("th"), 
                                 tabName = "about",
                                 badgeColor = "green"),
                        menuItem("Source Code", icon = icon("question-circle"), 
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
                                collapsible = TRUE)
                          ), 
                          fluidRow(
                            
                            # Visualizations Box
                            tabBox(width = 8,
                                   side = "left",
                                   title = "Visualizations", 
                                   id = "visulizationTabs", 
                                   tabPanel("Flow", plotOutput("flowVisualization")), 
                                   tabPanel("Ground Water", plotlyOutput("gwVisualization")), 
                                   tabPanel("Juvenile Salmon", plotOutput("screwTrapVisualization"))
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
                        # FlowWest About Page
                        tabItem(
                          tabName = "about",
                          tags$img(src="FlowWestLogo.png",
                                   width=250, height=73,
                                   style="display: block; margin-left: auto; margin-right: auto;"),
                          tags$br(),
                          includeHTML("flowwest_about.html")
                        ), 
                        # Raw Data Sources Page
                        tabItem(
                          tabName = "rawData", 
                          tags$h2("Let users look at and get raw data in this app")
                        )
                      )
                      
                    )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Render Leaflet MapView ===========================================================
  #
  output$mapView <- renderLeaflet({
    leaflet() %>%
      
      # set a group of basemaps to choose from 
      addTiles(group = "Open Maps Street Map (default)") %>%
      addProviderTiles("CartoDB.Positron", group = "Light Map") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      
      # map layers to choose from
      
      # well data layer shows up by default 
      addCircleMarkers(data = gwlLatLong,
                       clusterOptions = markerClusterOptions(),
                       lat = ~lat, 
                       lng = ~lng, 
                       stroke = TRUE, 
                       popup = "View Ground Water Visual below",
                       group = "Ground Water Wells") %>%
      
      # fish data can be added with a checkbox
      addMarkers(data = screwtrapLatLong, 
                 lng=~lng, 
                 lat=~lat,
                 group = "Screw Trap", 
                 popup = "View Screw Trap Visual Below",
                 layerId = "screwTrap_markers", 
                 icon = blueFishIcon) %>%
      
      # flow data marker
      addMarkers(data=flowLatLong, 
                 lat = ~lat, 
                 lng = ~lng, 
                 popup = "View Flow Visual Below",
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
  # =================================================================================
  #
  # Flow Data Visualization
  # Create an observer that observes for a clicked flow marker on the map
  observe({
    mapClick <- input$mapView_marker_click
    if (is.null(mapClick)) {
      return()
    } else if (mapClick[1] != "flow_markers") {
      return()
    } else {
      output$flowVisualization <- renderPlot({
        plot(1:100, sqrt(1:100))
      })
    }
  })
  
  
  # Groudwater Visualization
  #
  observe({
    mapClick <- input$mapView_marker_click
    if (is.null(mapClick)){
      return()
    }
    else 
      output$gwVisualization <- renderPlotly({
          gwlData %>%
          filter(LATITUDE == mapClick[3] & LONGITUDE == mapClick[4]) %>%
          plot_ly(x=~reading_date, y=~wse, type = "scatter", mode = "lines")
          
      })
  })
  
  # Screwtrap Visualization
  observe({
    mapClick <- input$mapView_marker_click
    if (is.null(mapClick)) {
      return()
    } else if (mapClick[1] != "screwTrap_markers") {
      return()
    } else {
      output$screwTrapVisualization <- renderPlot({
        plot(-100:100, (-100:100)^2)
      })
    }
  })
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)

