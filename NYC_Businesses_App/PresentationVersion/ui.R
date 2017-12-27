#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

shinyUI(dashboardPage(
   skin = 'black',
   dashboardHeader(title = "NYC Businesses",
                   titleWidth = 325),
   dashboardSidebar(
      width = 325,
      sidebarUserPanel(h4('Kathryn Bryant'),
                       image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
      sidebarMenu(
         menuItem(h4("License Exploration"), tabName = 'map', icon = icon('globe', "fa-2x")),
         menuItem(h4('Data'), tabName = 'data', icon = icon('table', "fa-2x"))
      ),
      #User chooses whether to explore instances of a single license or compare data for 
      #two different licenses.
      radioButtons('compare', h4('Analysis Preference'), c("Single License",
                                 "License Comparison"), selected = "Single License"),
      
      #One dropdown menu for licenses appears if user selects 'Single License' exploration
      #above or two dropdown menus for licenses appear if user selects 'License Comparison'.
      conditionalPanel("input.compare == 'Single License'",
                       selectInput('selected', h4('License Category to View'), industries,
                         selected = NULL)),
      conditionalPanel("input.compare == 'License Comparison'",
                      selectInput('first', h4('First License Category (in blue)'), industries,
                                  selected = "Amusement Arcade/Device"),
                      selectInput('second', h4('Second License Category (in orange)'), industries,
                                  selected = "Auction House Premises")),
      
      #User decides on level of granularity in exploration: either investigate by neighborhood
      #or zipcode. 
      radioButtons('buttons', h4('Explore by:'), c('Neighborhood', 'Zipcode'),
                   selected = 'Neighborhood'),
      
      #A dropdown menu appears for neighborhood or a text field appears for zipcode entry,
      #depending on the previous choice.
      conditionalPanel("input.buttons == 'Neighborhood'",
                  selectInput('neighborhood', h4('Neighborhood of Interest'), 
                              neighborhoods, selected = 'Airport')),
      conditionalPanel("input.buttons == 'Zipcode'",
                  textInput('zipcode', h4('Zip Code of Interest'), value = "",
                              placeholder = '10007')),
      
      #Lastly, users can look up what type of license a familiar business has.
      textInput('search', h4('License Lookup'), value = 'Kidtastix LLC',
                  placeholder = "Kidtastix LLC"),
      fluidRow(textOutput('businessinfo'), align = 'center')
   ),
   dashboardBody(
      tags$head(
         tags$link(rel = "stylesheet", type = 'text/css', href = "custom.css")
      ),
      tabItems( 
         tabItem(tabName = 'map', 
                 #First row gives a blurb about license requirements for the license(s)
                 #the user chooses. Only one row/infoBox appears if exploring 'Single License' 
                 #while two rows/infoBoxes appear if exploring 'License Comparison'.
                  fluidRow(conditionalPanel("input.compare == 'Single License'",
                                            infoBoxOutput("describe", width = 12)),
                           conditionalPanel("input.compare == 'License Comparison'",
                                            infoBoxOutput("describe1", width = 12))),
                 fluidRow(conditionalPanel("input.compare == 'License Comparison'",
                                           infoBoxOutput("describe2", width = 12))),
                 
                 #Second row contains the map and the borough summary graph if exploring
                 #'Single License' or it contains a larger version of just the map if
                 #exploring 'License Comparison'.
                  fluidRow(conditionalPanel("input.compare == 'Single License'",
                                            box(leafletOutput('map'), width = 6),
                                          box(plotOutput('boroughs'), width = 6)),
                           conditionalPanel("input.compare == 'License Comparison'",
                                            box(leafletOutput('map2'), width = 12))),
                 
                 #Third row contains a left infoBox with neighborhood/zipcode total and
                 #a right infoBox with the NYC total for the chosen license under 
                 #the 'Single License exploration or simply the first license chosen
                 #under the 'License Comparison' exploration.
                  fluidRow(conditionalPanel("input.compare == 'Single License'",
                                            conditionalPanel("input.buttons == 'Neighborhood'",
                                                infoBoxOutput('neigh', width = 6)),
                                            conditionalPanel("input.buttons == 'Zipcode'",
                                                infoBoxOutput('zipcode', width = 6)),
                                            infoBoxOutput('total', width = 6)),
                           conditionalPanel("input.compare == 'License Comparison'",
                                            conditionalPanel("input.buttons == 'Neighborhood'",
                                                infoBoxOutput('neigh1', width = 6)),
                                            conditionalPanel("input.buttons == 'Zipcode'",
                                                infoBoxOutput('zipcode1', width = 6)),
                                            infoBoxOutput('total1', width = 6))),
                 
                 #The last row contains either a long infoBox with business names in the
                 #chosen neighborhood/zipcode for the chosen license under the 'Single
                 #License' exploration, OR it contains the same two infoBoxes as the 
                 #previous row for the second chosen license under 'License Comparison'.
                 fluidRow(conditionalPanel("input.compare == 'Single License'",
                                 conditionalPanel("input.buttons == 'Neighborhood'",
                                                infoBoxOutput('neighnames', width = 12)),
                                          conditionalPanel("input.buttons == 'Zipcode'",
                                                infoBoxOutput('zipcodenames', width = 12))),
                          conditionalPanel("input.compare == 'License Comparison'",
                                            conditionalPanel("input.buttons == 'Neighborhood'",
                                                infoBoxOutput('neigh2', width = 6)),
                                            conditionalPanel("input.buttons == 'Zipcode'",
                                                infoBoxOutput('zipcode2', width = 6)),
                                            infoBoxOutput('total2', width = 6)))),
         
         #This tab contains an infoBox with a link to the data source website and
         #a scrollable/clickable copy of the data frame
         tabItem(tabName = 'data', fluidRow(infoBoxOutput('pdf', width = 12)),
                 fluidRow(box(DT::dataTableOutput('table'), 
                                                width = 12)))
      )
   )
)
)
