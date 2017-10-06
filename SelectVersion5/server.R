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
library(RColorBrewer)




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
         addMarkers(~Longitude, ~Latitude, popup = ~Detail,
                    label = ~Name) 
   }) 
    
    
    output$boroughs = renderPlot({
       DCA_grouped = DCA %>% filter(Industry == input$selected) %>%
          group_by(Borough)
   ggplot(data = DCA_grouped, aes(x = Borough)) +
          geom_bar(aes(fill = Borough)) + ylab("Number of Businesses") +
      scale_x_discrete(drop = FALSE) +
      theme_economist() + scale_fill_economist() +
      ggtitle('Number of Businesses by Borough')
          
    }) #option?: use fill = Postcode or License.Type instead
    
        #InfoBoxes won't scale horizontally :(
    output$total = renderInfoBox({
       DCA_total = DCA %>% filter(Industry %in% input$selected) %>%
          summarise(Total = n())
       infoBox('Total in NYC', DCA_total$Total, 
                icon = icon('calculator'), fill = TRUE, width = 6,
               color = 'blue')
       
    }) #cat('Total', input$selected, 'Businesses')
    
   output$zipcode = renderInfoBox({
       DCA_zip_total = DCA %>% filter(Industry %in% input$selected, 
                                  Postcode == input$zipcode) %>%
          summarise(Total = n())
       infoBox(paste(c('Total in', as.character(input$zipcode)), collapse = ' '), 
               DCA_zip_total$Total, icon = icon('envelope-o'), 
               fill = TRUE, width = 6, color = 'teal')
       
    })
    
    output$describe = renderInfoBox({
       Single_description = DCA_Describe %>% 
          filter(Industry %in% input$selected)
       infoBox(paste(c(input$selected, 'licensing details:'), collapse = ' '),
               Single_description$Descriptions,
               icon = icon('vcard-o'), fill = FALSE, width = 12,
               color = 'navy')
    })
    
    output$comparisons = renderPlotly({
       pie_data = DCA %>% filter(Postcode == input$zipcode) %>%
         group_by(Industry) %>% mutate(Count = n())
       
       plot_ly(pie_data, labels = ~Industry, values = ~Count, type = 'pie',
            marker = list(color = colorRampPalette(brewer.pal(9, "GnBu"))(34)),
            textposition = 'inside', textinfo = 'label+percent',
            hoverinfo = 'text', text = ~paste(Count, 'businesses')) %>%
         layout(title = paste('Distribution of License Categories in', 
                              input$zipcode, collapse = ' '),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
     
    
    #output$comparisons = renderPlot({
    #  pie_data = DCA %>% filter(Postcode == input$zipcode) %>%
    #     group_by(Industry) %>% mutate(Count = n())
       
    #   ggplot(pie_data, aes(x = '', y = Count, fill = Industry)) +
    #      geom_bar(width = 1, stat = 'identity') + coord_polar('y') +
    #      theme_economist(horizontal = FALSE) + ggtitle(paste(c('Licenses in', input$zipcode), 
    #                                        collapse = ' ')) +
    #      theme(legend.position = 'bottom') + xlab('') + ylab('')})
    
    
    output$table = DT::renderDataTable({
       DCA_Clean = DCA %>% select(Name, License.Type, Industry, Postcode,
                        Longitude, Latitude, Borough)
       datatable(DCA, rownames = FALSE, options = list(scrollX = TRUE)) 
    })
    
   

    
})
