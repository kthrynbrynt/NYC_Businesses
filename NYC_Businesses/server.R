#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(dplyr)
library(ggplot2)
library(leaflet)
library(googleVis)
library(DT)
library(shinydashboard)
library()



shinyServer(function(input, output, session) {
   output$map = renderLeaflet({
      leaflet(DCA) %>% 
      addProviderTiles('Esri.WorldStreetMap') %>%
      setView(-73.968285, 40.785091, 12) 
   })
   
    observeEvent(input$checked, {
      DCA_updated = DCA %>% filter(Category %in% input$checked)
      proxy <- leafletProxy("map", data = DCA_updated)
      proxy %>% addMarkers(~Longitude, ~Latitude)
   }, ignoreInit = TRUE)
   
})

