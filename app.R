#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(googleway)
library(mapsapi)
library(leaflet)
library(randomcoloR)
library(dplyr)



ui <- fluidPage(titlePanel("Finding Directions"), 
                sidebarLayout(
                  sidebarPanel(
                    textInput(inputId = "origin", label = "Leaving Spot"),
                    textInput(inputId = "destination", label = "Destination"),
                    actionButton(inputId = "route", label = "Find Route",
                                 )),
                  
                  mainPanel("Google Map",
                            google_mapOutput(outputId = "ggmap"))
                )
)




server <- function(input, output, session) {
  
  api_key <- "AIzaSyA1RNI__oaXt7ZaMoBoeMqF2zEladTuDpw"
  
  output$gmap <- renderGoogle_map({
    google_map(key = api_key,
               location = c(42.3555275, -71.0565313),
               zoom = 10,
               search_box = TRUE,
               scale_control = TRUE,
               height = 1000)%>%
      add_traffic()
  })
  
  observeEvent(input$route,{
    
    r <- mp_directions(origin = input$origin,
                       destination = input$destination,
                       alternatives = TRUE,
                       key = api_key,
                       quiet = TRUE,
                       transit_mode = c("bus", "subway"))
    rd<- mp_get_routes(r)
    i <- nrow(rd)
    rd$color <- c(randomColor(count = i, luminosity = "bright"))
    
    google_map_update(map_id = "ggmap") %>%
      clear_traffic() %>%
      clear_polylines() %>%
      clear_markers() %>%
      add_traffic() %>%
      add_polylines(data = rd,
                    polyline = "geometry",
                    stroke_colour = "color",
                    stroke_weight = 5,
                    stroke_opacity = 0.9,
                    info_window = "duration_text",                    
                    load_interval = 100)
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
