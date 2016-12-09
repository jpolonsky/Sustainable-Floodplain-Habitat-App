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
library(dplyr)

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
                        menuItem("About FlowWest", icon = icon("question-circle"), 
                                 tabName = "about",
                                 badgeColor = "green"),
                        menuItem("Source Code", icon = icon("github"), 
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
                                   title = tagList(shiny::icon("area-chart"), "Visualizations"), 
                                   id = "visulizationTabs", 
                                   tabPanel("Flow", plotlyOutput("flowVisualization")), 
                                   tabPanel("Flow Prediction", plotOutput("flowPredictions")),
                                   tabPanel("Juvenile Salmon", plotOutput("screwTrapVisualization")),
                                   tabPanel("Groundwater", plotlyOutput("gwVisualization"))
                            ),
                            
                            # Analytic Results 
                            column(width = 4,
                                   tabBox(
                                     width = 12, 
                                     side = "left", 
                                     title = tagList(icon("pie-chart"), "Insights"), 
                                     id = "insighTab",
                                     tabPanel("Flow",
                                              dateInput("dateSelect", label = "Select a Custom Date",
                                                        min = "2015-01-01", max = "2016-12-04",
                                                        format = "yyyy-mm-dd", value = "2016-11-23"),
                                              
                                              fluidRow(
                                                infoBoxOutput("flowThreshold", width = 12) 
                                              ),
                                              fluidRow(
                                                infoBoxOutput("flowTodaysNeed", width = 12)
                                              ),
                                              fluidRow(
                                                infoBoxOutput("flowNaturalFlow", width = 12)
                                              )
                                     ), 
                                     tabPanel("Juvenile Salmon", 
                                              dateInput("dateSelectFish", label = "Select a Custom Date",
                                                        min = "1995-01-01", max = "2016-12-31",
                                                        format = "yyyy-mm-dd", value = "2016-12-01"),
                                              fluidRow(
                                                infoBoxOutput("screwTrapMetric1", width = 12) 
                                              ),
                                              fluidRow(
                                                infoBoxOutput("screwTrapMetric2", width = 12)
                                              )),
                                     tabPanel("Groundwater",
                                              selectInput("smoothSelect", 
                                                          label = "Select a Smoother (Loess is Default)", 
                                                          choices = smoothersList, 
                                                          selected = "loess"),
                                              fluidRow(
                                                infoBoxOutput("gwMetric1", width = 12) 
                                              )) 
                                     
                                   )
                                   
                            )
                            
                          )
                        ), 
                        # FlowWest About Page
                        tabItem(
                          tabName = "about",
                          tags$a(href="http://www.flowwest.com/",
                          tags$img(src="FlowWestLogo.png",
                                   width=250, height=73,
                                   style="display: block; margin-left: auto; margin-right: auto;"),
                          target="_blank"),
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
                       clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = TRUE),
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
                 popup = "Juvenile Salmon Visual Below",
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
  
  # Flow Infoboxes -----------------------------------------------------------------
  output$flowThreshold <- renderInfoBox({
    infoBox(
      "Flodplain inundation threshold", Qmetric(input$dateSelect)$Threshold, "cfs",
      color = "green", 
      fill = TRUE
    )
  })
  output$flowTodaysNeed <- renderInfoBox({
    infoBox(
      "Additional Flow Needed To Inundate", Qmetric(input$dateSelect)$NeedToday, "cfs", 
      color = paste0(TodaysNeedColor(input$dateSelect)), fill = TRUE
    )
  })
  output$flowNaturalFlow <- renderInfoBox({
    infoBox(
      "Predicted flow will inundate in", paste(GetDaysUntilThreshold(input$dateSelect, 20000),"Days", sep = " "),
      color = "orange", fill = TRUE
    )
  })
  
  # Groundwater Infoboxes --------------------------------------------------------
  output$gwMetric1 <- renderInfoBox({
    infoBox("Smoother applied to data:", input$smoothSelect,fill = TRUE)
  })
  
  output$gwMetric2 <- renderInfoBox({
    infoBox("Ground Water Metrics 2", fill = TRUE)
  })
  
  # ScrewTrap Infoboxes -----------------------------------------------------------
  output$screwTrapMetric1 <- renderInfoBox({
    infoBox("Number of Juvenile Salmon available for floodplain:", 
            paste0(GetSalmonAgg(input$dateSelectFish), " Counted Fish"),
            fill = TRUE, color ="olive")
  })
  
  output$screwTrapMetric2 <- renderInfoBox({
    infoBox("Proporion of year class present: ", 
            paste0(GetHistoricalPercent(input$dateSelectFish), "%"),
            fill = TRUE, color ="yellow")
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
      output$flowVisualization <- renderPlotly({
        flowData %>%
          plot_ly(x=~Date, y=~mean_daily, 
                  type = "scatter", 
                  mode = "lines", 
                  name = "Flow", 
                  text = ~paste("Daily Mean: ", mean_daily)) %>%
          add_trace(x=input$dateSelect, type ="scatter", name=paste0(input$dateSelect)) %>%
          layout(xaxis = list(title="Date"), yaxis= list(title="Mean Daily Flow (cfs)"))
      })
    }
  })
  
  
  # Groudwater Visualization
  # Observer observes for the 
  observe({
    mapClick <- input$mapView_marker_click
    if (is.null(mapClick)){
      return()
    }
    else 
      output$gwVisualization <- renderPlotly({
        p <- gwlData %>%
          filter(LATITUDE == mapClick[3] & LONGITUDE == mapClick[4]) %>%
          ggplot(aes(reading_date, wse)) + 
          geom_point(aes(reading_date, wse)) +
          geom_line(aes(reading_date, wse)) +
          geom_smooth(method = input$smoothSelect) + 
          xlab("Reading Date") + ylab("Water Surface Elevation ft.")
        
          ggplotly(p)
        
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
        screwTrapData %>%
          filter(FinalRun != "Unassigned", SampleDate_formated > as.Date("2004-01-01")) %>%
          sample_frac(.35) %>%
          ggplot(aes(SampleDate_formated, Unmarked, color=FinalRun)) + geom_point() + facet_grid(FinalRun ~ .) + 
          xlab("Date") + ylab("Juvenile Salmon Count")
        
      })
    }
  })
  
  # flow predictions 
  observe({
    mapClick <- input$mapView_marker_click
    if (is.null(mapClick)) {
      return()
    } else if (mapClick[1] != "flow_markers") {
      return()
    } else {
      output$flowPredictions <- renderPlot({
        p <- ggplot(data = shasta_preds_v) + geom_line(aes(x=start, y=value, color = key, group = start)) + 
          xlab("Date") + guides(color = guide_legend(title = "Predicted Probability")) + 
          ylab("Flow (cfs)")
        print(p)
      })
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

