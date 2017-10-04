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
library(ggthemes)



shinyServer(function(input, output, session) {
   output$map = renderLeaflet({
      leaflet() %>% 
      addProviderTiles('Esri.WorldStreetMap') %>%
      setView(-73.968285, 40.785091, 10) 
   })
   
   
    observeEvent(input$selected, {
      DCA_add = DCA %>% filter(Industry %in% input$selected)
      leafletProxy("map", data = DCA_add) %>%
         clearMarkers() %>%
         addMarkers(~Longitude, ~Latitude, markerOptions(clickable = TRUE),
                       icon = icon(DCA_add$Icon[1])) #icons don't change :(
   }) 
    
    
    output$boroughs = renderPlot({
       DCA_grouped = DCA %>% filter(Industry == input$selected) %>%
          group_by(Borough)
   ggplot(data = DCA_grouped, aes(x = Borough)) +
          geom_bar(aes(fill = Borough)) + ylab("Number of Businesses") +
      theme_economist() + scale_fill_economist() +
      ggtitle('Number of Businesses by Borough')
          
    }) #option?: use fill = Postcode or License.Type instead
    
    
})
