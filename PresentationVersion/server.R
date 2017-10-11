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

   #Controls map for 'Single License' exploration
   output$map = renderLeaflet({
      DCA_start = DCA %>% filter(Industry == 'Amusement Arcade/Device')
      leaflet(DCA_start) %>% 
      addProviderTiles('Esri.WorldStreetMap') %>%
      setView(-73.968285, 40.785091, 10) %>%
      addMarkers(~Longitude, ~Latitude, 
                    popup = paste(c(~NTA, ~Detail), collapse = ';'),
                    label = ~Name)
   })
   
   #Controls map for 'License Comparison' exploration
   output$map2 = renderLeaflet({
      leaflet() %>% 
      addProviderTiles('Esri.WorldStreetMap') %>%
      setView(-73.968285, 40.785091, 9) 
      # addMarkers(~Longitude, ~Latitude, 
      #               popup = paste(c(~NTA, ~Detail), collapse = ';'),
      #               label = ~Name)
   })
   
   
   #The following code makes it so that the 'Single License' map has
   #markers on businesses having the user-chosen license; it also makes
   #it so that the 'License Comparison' map has markers in blue on 
   #businesses having the first user-chosen license and markers in 
   #orange on businesses having the second user-chosen license.
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
         leafletProxy("map2") %>%
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
   
   #Creates the barplot that displays the count distribution for the user-
   #chosen licenses across the five boroughs. It was important for me to have
   #all boroughs show up, even when the counts were zero for visual 
   #consistency.
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
    
    
   #InfoBox about NYC total for single chosen license
    output$total = renderInfoBox({
       DCA_total = DCA %>% filter(Industry %in% input$selected) %>%
          summarise(Total = n())
       infoBox(h4('Total', input$selected, 'licenses in NYC'), DCA_total$Total, 
                icon = icon('calculator'), fill = TRUE, width = 6,
               color = 'teal')
       
    }) 
    
   #InfoBox about NYC total for first chosen license
   output$total1 = renderInfoBox({
       DCA_total = DCA %>% filter(Industry %in% input$first) %>%
          summarise(Total = n())
       infoBox(h4('Total', input$selected, 'licenses in NYC'), DCA_total$Total, 
                icon = icon('calculator'), fill = TRUE, width = 6,
               color = 'light-blue')
       
    })
   
   #InfoBox about NYC total for second chosen license   
   output$total2 = renderInfoBox({
       DCA_total = DCA %>% filter(Industry %in% input$second) %>%
          summarise(Total = n())
       infoBox(h4('Total', input$second, 'licenses in NYC'), DCA_total$Total, 
                icon = icon('calculator'), fill = TRUE, width = 6,
               color = 'yellow')
       
    })
    
   #InfoBox about user-chosen neighborhood total for single chosen license
   output$neigh = renderInfoBox({
       DCA_neighborhood = DCA %>% filter(Industry %in% input$selected, 
                                  NTA == input$neighborhood) %>%
          summarise(Total = n())
       infoBox(h4(paste(c('Total', input$selected, 'licenses in', as.character(input$neighborhood)), collapse = ' ')), 
               DCA_neighborhood$Total, icon = icon('calculator'), 
               fill = TRUE, width = 6, color = 'teal')
       
    })
   
   #InfoBox about user-chosen neighborhood total for first chosen license
   output$neigh1 = renderInfoBox({
       DCA_neighborhood = DCA %>% filter(Industry %in% input$first, 
                                  NTA == input$neighborhood) %>%
          summarise(Total = n())
       infoBox(h4(paste(c('Total', input$first, 'licenses in', as.character(input$neighborhood)), collapse = ' ')), 
               DCA_neighborhood$Total, icon = icon('calculator'), 
               fill = TRUE, width = 6, color = 'light-blue')
       
    })

   #InfoBox about user-chosen neighborhood total for second chosen license
   output$neigh2 = renderInfoBox({
       DCA_neighborhood = DCA %>% filter(Industry %in% input$second, 
                                  NTA == input$neighborhood) %>%
          summarise(Total = n())
       infoBox(h4(paste(c('Total', input$second, 'licenses in', as.character(input$neighborhood)), collapse = ' ')), 
               DCA_neighborhood$Total, icon = icon('calculator'), 
               fill = TRUE, width = 6, color = 'yellow')
       
    })
   
   #InfoBox about user-chosen zipcode total for single chosen license
   output$zipcode = renderInfoBox({
       DCA_zip_total = DCA %>% filter(Industry == input$selected, 
                                  Postcode == input$zipcode) %>%
          summarise(Total = n())
       infoBox(h4(paste(c('Total', input$selected, 'licenses in', as.character(input$zipcode)), collapse = ' ')), 
               DCA_zip_total$Total, icon = icon('calculator'), 
               fill = TRUE, width = 6, color = 'teal')
       
    })

   #InfoBox about user-chosen zipcode total for first chosen license
   output$zipcode1 = renderInfoBox({
       DCA_zip_total = DCA %>% filter(Industry == input$first, 
                                  Postcode == input$zipcode) %>%
          summarise(Total = n())
       infoBox(h4(paste(c('Total', input$first, 'licenses in', as.character(input$zipcode)), collapse = ' ')), 
               DCA_zip_total$Total, icon = icon('calculator'), 
               fill = TRUE, width = 6, color = 'light-blue')
       
    })

   #InfoBox about user-chosen zipcode total for second chosen license
   output$zipcode2 = renderInfoBox({
       DCA_zip_total = DCA %>% filter(Industry == input$second, 
                                  Postcode == input$zipcode) %>%
          summarise(Total = n())
       infoBox(h4(paste(c('Total', input$second, 'licenses in', as.character(input$zipcode)), collapse = ' ')), 
               DCA_zip_total$Total, icon = icon('calculator'), 
               fill = TRUE, width = 6, color = 'yellow')
       
    })
   
   #InfoBox containing the names of all the businesses with the chosen license
   #in the chosen neighborhood
   output$neighnames = renderInfoBox({
      DCA_neigh_names = DCA %>% filter(Industry == input$selected, 
                                  NTA == input$neighborhood) %>%
                           select(Name)
      unique_neigh_names = unique.data.frame(DCA_neigh_names)
      infoBox(h4(paste(c(input$selected, "businesses in", input$neighborhood), collapse = ' ')),
              paste(unique_neigh_names$Name, collapse = '; '), 
              icon = icon('handshake-o'), fill = TRUE, width = 12,
              color = 'light-blue')
   })
   
   #InfoBox containing the names of all the businesses with the chosen license
   #in the chosen zipcode
   output$zipcodenames = renderInfoBox({
      DCA_zip_names = DCA %>% filter(Industry == input$selected, 
                                  Postcode == input$zipcode) %>%
                           select(Name)
      unique_zip_names = unique.data.frame(DCA_zip_names)
      infoBox(h4(paste(c(input$selected, 'businesses in', input$neighborhood), collapse = ' ')),
              paste(unique_zip_names$Name, collapse = '; '), 
              icon = icon('handshake-o'), fill = TRUE, width = 12,
              color = 'light-blue')
   })
   
   #InfoBox with the licensing information blurb for the chosen license
    output$describe = renderInfoBox({
       Single_description = DCA_Describe %>% 
          filter(Industry %in% input$selected)
       infoBox(h4(paste(c(input$selected, 'licensing details:'), collapse = ' ')),
               Single_description$Descriptions,
               icon = icon('vcard-o'), fill = FALSE, width = 12,
               color = 'light-blue')
    })
    
    #InfoBox with the licensing information blurb for the first chosen license
    output$describe1 = renderInfoBox({
       Single_description = DCA_Describe %>% 
          filter(Industry %in% input$first)
       infoBox(h4(paste(c(input$first, 'licensing details:'), collapse = ' ')),
               Single_description$Descriptions,
               icon = icon('vcard-o'), fill = FALSE, width = 12,
               color = 'light-blue')
    })

    #InfoBox with the licensing information blurb for the second chosen license
    output$describe2 = renderInfoBox({
       Single_description = DCA_Describe %>% 
          filter(Industry %in% input$second)
       infoBox(h4(paste(c(input$second, 'licensing details:'), collapse = ' ')),
               Single_description$Descriptions,
               icon = icon('vcard-o'), fill = FALSE, width = 12,
               color = 'yellow')
    })
    
    
    #Code that returns the license type of a business when the user
    #searches a business name; uses a small bit of regular expressions
    output$businessinfo = renderPrint({
       DCA_search = DCA %>% select(Name, Industry)
       indices = grep(paste(c(input$search, '+'), collapse = ''),
                      DCA_search$Name, ignore.case = TRUE)
       result = unique.data.frame(DCA_search[indices,])
      result$Industry = as.character(result$Industry)
      unique_result = unique(result$Industry)
      cat('Licence(s):', unique_result, sep = '\n')
   }) #NY Carousel Entertainment
    
    #InfoBox on the second tab providing the link the the original data source
    output$pdf = renderInfoBox({
       infoBox(h4('Original data and documentation from NYC Open Data available here:'), 
               tagList(a("NYC Open Data: Legally Operating Businesses", href = "https://data.cityofnewyork.us/Business/Legally-Operating-Businesses/w7w3-xahh")),
               fill = FALSE, width = 12, color = 'light-blue')
    })

    #Code that produces the data table in the second tab
    output$table = DT::renderDataTable({
       DCA_Clean = DCA %>% select(Name, License.Type, Industry, Postcode,
                        Longitude, Latitude, Borough)
       datatable(DCA, rownames = FALSE, options = list(scrollX = TRUE)) 
  })
    
    
    
#    Unused code that produces a piechart of the different percentages of
#    each license based on user-chosen zipcode
    
#    output$comparisons = renderPlot({
#       pie_data = DCA %>% filter(Postcode == input$zipcode) %>%
#         group_by(Industry) %>% mutate(Count = n())
       
#       ggplot(pie_data, aes(x = '', y = Count, fill = Industry)) +
#          geom_bar(width = 1, stat = 'identity') + coord_polar('y') +
#          theme_economist(horizontal = FALSE) + ggtitle(paste(c('Licenses in', input$zipcode), 
#                                            collapse = ' ')) +
#          theme(legend.position = 'bottom') + xlab('') + ylab('')})   

    
})
