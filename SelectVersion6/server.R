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
      DCA_start = DCA %>% filter(Industry == 'Amusement Arcade/Device')
      leaflet(DCA_start) %>% 
      addProviderTiles('Esri.WorldStreetMap') %>%
      setView(-73.968285, 40.785091, 10) %>%
      addMarkers(~Longitude, ~Latitude, 
                    popup = paste(c(~NTA, ~Detail), collapse = ';'),
                    label = ~Name)
   })
   
   
    observeEvent(input$selected, {
      DCA_add = DCA %>% filter(Industry %in% input$selected)
      leafletProxy("map", data = DCA_add) %>%
         clearMarkers() %>%
         addAwesomeMarkers(~Longitude, ~Latitude, 
                    popup = paste("Neighborhood:", DCA_add$NTA, "<br>",
                                  "Zipcode", DCA_add$Postcode, "<br>",
                                  "", DCA_add$Detail),
                    label = ~Name, icon = icons) 
   })
    

### This chunk doesn't react as I want it to. I want to simply remove the
  # markers from the existing map in the selected neighborhood and replace
  # them with differently colored ones.
   eventReactive(input$neighborhood, {
      DCA_nbhd_color = DCA %>% filter(Industry == input$selected, 
                                      NTA == input$neighborhood)
      leafletProxy("map", data = DCA_nbhd_color) %>%
         clearMarkers() %>%
         addAwesomeMarkers(~Longitude, ~Latitude, 
                    popup = paste("Neighborhood:", DCA_nbhd_color$NTA, "<br>",
                                  "Zipcode", DCA_add$Postcode, "<br>",
                                  "", DCA_nbhd_color$Detail),
                    label = ~Name, icon = icons_added) 
   })
    
    
    output$boroughs = renderPlot({
       DCA_grouped = DCA %>% filter(Industry == input$selected) %>%
          group_by(Borough)
      ggplot(data = DCA_grouped, aes(x = Borough)) +
          geom_bar(aes(fill = Borough)) + ylab("Number of Businesses") +
      scale_x_discrete(drop = FALSE) +
      theme_economist() + scale_fill_economist() +
      ggtitle(paste(c('Number of', input$selected, 'Businesses by Borough'), 
                    collapse = ' ')) +
      theme(legend.position = 'none')
          
    }) 
    
    

    output$total = renderInfoBox({
       DCA_total = DCA %>% filter(Industry %in% input$selected) %>%
          summarise(Total = n())
       infoBox(h4('Total in NYC'), DCA_total$Total, 
                icon = icon('calculator'), fill = TRUE, width = 6,
               color = 'teal')
       
    }) 
    

   output$neigh = renderInfoBox({
       DCA_neighborhood = DCA %>% filter(Industry %in% input$selected, 
                                  NTA == input$neighborhood) %>%
          summarise(Total = n())
       infoBox(h4(paste(c('Total in', as.character(input$neighborhood)), collapse = ' ')), 
               DCA_neighborhood$Total, icon = icon('envelope-o'), 
               fill = TRUE, width = 6, color = 'blue')
       
    })
   
   
   output$zipcode = renderInfoBox({
       DCA_zip_total = DCA %>% filter(Industry %in% input$selected, 
                                  Postcode == input$zipcode) %>%
          summarise(Total = n())
       infoBox(paste(c('Total in', as.character(input$zipcode)), collapse = ' '), 
               DCA_zip_total$Total, icon = icon('envelope-o'), 
               fill = TRUE, width = 6, color = 'blue')
       
    })
   

    output$describe = renderInfoBox({
       Single_description = DCA_Describe %>% 
          filter(Industry %in% input$selected)
       infoBox(paste(c(input$selected, 'licensing details:'), collapse = ' '),
               Single_description$Descriptions,
               icon = icon('vcard-o'), fill = FALSE, width = 12,
               color = 'navy')
    })
    

    
    output$table = DT::renderDataTable({
       DCA_Clean = DCA %>% select(Name, License.Type, Industry, Postcode,
                        Longitude, Latitude, Borough)
       datatable(DCA, rownames = FALSE, options = list(scrollX = TRUE)) 
  })
    
    
    
    
#    output$comparisons = renderPlot({
#       pie_data = DCA %>% filter(Postcode == input$zipcode) %>%
#         group_by(Industry) %>% mutate(Count = n())
       
#       ggplot(pie_data, aes(x = '', y = Count, fill = Industry)) +
#          geom_bar(width = 1, stat = 'identity') + coord_polar('y') +
#          theme_economist(horizontal = FALSE) + ggtitle(paste(c('Licenses in', input$zipcode), 
#                                            collapse = ' ')) +
#          theme(legend.position = 'bottom') + xlab('') + ylab('')})   

    
})
