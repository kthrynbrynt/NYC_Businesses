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
      leaflet() %>% 
      addProviderTiles('Esri.WorldStreetMap') %>%
      setView(-73.968285, 40.785091, 12) 
   })
   
   values = reactiveValues()
   values$checked = c()
   
    observeEvent(input$checked, {
      to_add = setdiff(input$checked, intersect(values$checked, input$checked))
      DCA_add = DCA %>% filter(Category %in% to_add)
      leafletProxy("map", data = DCA_add) %>% 
         addMarkers(~Longitude, ~Latitude)
      values$checked = input$checked
   }) 
    
    
})

