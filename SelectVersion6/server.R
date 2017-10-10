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
   
   
   observeEvent({
      input$compare
      input$selected
      input$first
      input$second
      }, {
      if (input$compare == "Single License") {
         DCA_add = DCA %>% filter(Industry %in% input$selected)
         leafletProxy("map", data = DCA_add) %>%
            clearMarkers() %>%
            addAwesomeMarkers(~Longitude, ~Latitude, 
                       popup = paste("Neighborhood:", DCA_add$NTA, "<br>",
                                     "Zipcode:", DCA_add$Postcode, "<br>",
                                     "", DCA_add$Detail),
                       label = ~Name, icon = icons) 
      } else {
         DCA_first = DCA %>% filter(Industry == input$first)
         DCA_second = DCA %>% filter(Industry == input$second)
         leafletProxy("map") %>%
             clearMarkers() %>%
            addAwesomeMarkers(DCA_first$Longitude, DCA_first$Latitude,
                    popup = paste("Neighborhood:", DCA_first$NTA, "<br>",
                                 "Zipcode:", DCA_first$Postcode, "<br>",
                                  "", DCA_first$Detail),
                    label = DCA_first$Name, icon = icons, group = 'first') %>%
            addAwesomeMarkers(DCA_second$Longitude, DCA_second$Latitude,
                    popup = paste("Neighborhood:", DCA_second$NTA, "<br>",
                                  "Zipcode:", DCA_second$Postcode, "<br>",
                                  "", DCA_second$Detail),
                    label = DCA_second$Name, icon = icons_added, group = 'second')
      }
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
       infoBox(h4('Total', input$selected, 'licenses in NYC'), DCA_total$Total, 
                icon = icon('calculator'), fill = TRUE, width = 6,
               color = 'teal')
       
    }) 
    
   output$total1 = renderInfoBox({
       DCA_total = DCA %>% filter(Industry %in% input$first) %>%
          summarise(Total = n())
       infoBox(h4('Total', input$selected, 'licenses in NYC'), DCA_total$Total, 
                icon = icon('calculator'), fill = TRUE, width = 6,
               color = 'teal')
       
    })
      
   output$total2 = renderInfoBox({
       DCA_total = DCA %>% filter(Industry %in% input$second) %>%
          summarise(Total = n())
       infoBox(h4('Total', input$second, 'licenses in NYC'), DCA_total$Total, 
                icon = icon('calculator'), fill = TRUE, width = 6,
               color = 'blue')
       
    })
    

   output$neigh = renderInfoBox({
       DCA_neighborhood = DCA %>% filter(Industry %in% input$selected, 
                                  NTA == input$neighborhood) %>%
          summarise(Total = n())
       infoBox(h4(paste(c('Total', input$selected, 'licenses in', as.character(input$neighborhood)), collapse = ' ')), 
               DCA_neighborhood$Total, icon = icon('calculator'), 
               fill = TRUE, width = 6, color = 'teal')
       
    })
   
   output$neigh1 = renderInfoBox({
       DCA_neighborhood = DCA %>% filter(Industry %in% input$first, 
                                  NTA == input$neighborhood) %>%
          summarise(Total = n())
       infoBox(h4(paste(c('Total', input$first, 'licenses in', as.character(input$neighborhood)), collapse = ' ')), 
               DCA_neighborhood$Total, icon = icon('calculator'), 
               fill = TRUE, width = 6, color = 'teal')
       
    })

   output$neigh2 = renderInfoBox({
       DCA_neighborhood = DCA %>% filter(Industry %in% input$second, 
                                  NTA == input$neighborhood) %>%
          summarise(Total = n())
       infoBox(h4(paste(c('Total', input$second, 'licenses in', as.character(input$neighborhood)), collapse = ' ')), 
               DCA_neighborhood$Total, icon = icon('calculator'), 
               fill = TRUE, width = 6, color = 'blue')
       
    })
   
   
   output$zipcode = renderInfoBox({
       DCA_zip_total = DCA %>% filter(Industry == input$selected, 
                                  Postcode == input$zipcode) %>%
          summarise(Total = n())
       infoBox(h4(paste(c('Total', input$selected, 'licenses in', as.character(input$zipcode)), collapse = ' ')), 
               DCA_zip_total$Total, icon = icon('calculator'), 
               fill = TRUE, width = 6, color = 'teal')
       
    })

   output$zipcode1 = renderInfoBox({
       DCA_zip_total = DCA %>% filter(Industry == input$first, 
                                  Postcode == input$zipcode) %>%
          summarise(Total = n())
       infoBox(h4(paste(c('Total', input$first, 'licenses in', as.character(input$zipcode)), collapse = ' ')), 
               DCA_zip_total$Total, icon = icon('calculator'), 
               fill = TRUE, width = 6, color = 'teal')
       
    })

   output$zipcode2 = renderInfoBox({
       DCA_zip_total = DCA %>% filter(Industry == input$second, 
                                  Postcode == input$zipcode) %>%
          summarise(Total = n())
       infoBox(h4(paste(c('Total', input$second, 'licenses in', as.character(input$zipcode)), collapse = ' ')), 
               DCA_zip_total$Total, icon = icon('calculator'), 
               fill = TRUE, width = 6, color = 'blue')
       
    })
   
   
   output$neighnames = renderInfoBox({
      DCA_neigh_names = DCA %>% filter(Industry == input$selected, 
                                  NTA == input$neighborhood) %>%
                           select(Name)
      unique_neigh_names = unique.data.frame(DCA_neigh_names)
      infoBox(h4(paste(c(input$selected, "businesses in", input$neighborhood), collapse = ' ')),
              paste(unique_neigh_names$Name, collapse = '; '), 
              icon = icon('handshake-o'), fill = TRUE, width = 12,
              color = 'blue')
   })
   
   
   output$zipcodenames = renderInfoBox({
      DCA_zip_names = DCA %>% filter(Industry == input$selected, 
                                  Postcode == input$zipcode) %>%
                           select(Name)
      unique_zip_names = unique.data.frame(DCA_zip_names)
      infoBox(h4(paste(c(input$selected, 'businesses in', input$neighborhood), collapse = ' ')),
              paste(unique_zip_names$Name, collapse = '; '), 
              icon = icon('handshake-o'), fill = TRUE, width = 12,
              color = 'blue')
   })
   

    output$describe = renderInfoBox({
       Single_description = DCA_Describe %>% 
          filter(Industry %in% input$selected)
       infoBox(h4(paste(c(input$selected, 'licensing details:'), collapse = ' ')),
               Single_description$Descriptions,
               icon = icon('vcard-o'), fill = FALSE, width = 12,
               color = 'blue')
    })
    
    
    output$businessinfo = renderPrint({
       DCA_search = DCA %>% select(Name, Industry)
       indices = grep(paste(c(input$search, '+'), collapse = ''),
                      DCA_search$Name, ignore.case = TRUE)
       result = unique.data.frame(DCA_search[indices,])
      result$Industry = as.character(result$Industry)
      unique_result = unique(result$Industry)
      cat(unique_result, sep = '\n')
   }) #NY Carousel Entertainment
    
    
    output$pdf = renderInfoBox({
       infoBox(h4('Original data and documentation from NYC Open Data available here:'), 
               tagList(a("NYC Open Data: Legally Operating Businesses", href = "https://data.cityofnewyork.us/Business/Legally-Operating-Businesses/w7w3-xahh")),
               fill = FALSE, width = 12, color = 'blue')
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
