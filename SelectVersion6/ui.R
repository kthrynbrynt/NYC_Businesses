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
         #menuItem('Questions and Motivation', tabName = 'questions', icon = icon('lightbulb')),
         menuItem(h4("License Exploration"), tabName = 'map', icon = icon('globe', "fa-2x")),
         #menuItem("License Comparisons", tabName = 'comparisons', icon = icon('pie-chart')),
         menuItem(h4('Data'), tabName = 'data', icon = icon('table', "fa-2x"))
        # menuItem('Income Data', tabName = 'data2', icon = icon('table'))
      ),
      #radioButtons('compare', h4('Analysis Preference'), c("Single License",
      #                           "License Comparison"), selected = "Single License"),
      #conditionalPanel("input.compare == 'Single License'",
      #                 selectInput('selected', h4('License Category to View'), industries, 
      #                   selected = NULL)),
      #conditionalPanel("input.compare == 'License Comparison'",
      #                selectInput('first', h4('First License Category'), industries,
      #                            selected = "Amusement Arcade/Device"),
      #                selectInput('second', h4('Second License Category'), industries,
      #                            selected = "Auction House Premises")),
      selectInput('selected', h4('License Category to View'), industries, 
                         selected = NULL),
      radioButtons('buttons', h4('Explore by:'), c('Neighborhood', 'Zipcode'),
                   selected = 'Neighborhood'),
      conditionalPanel("input.buttons == 'Neighborhood'",
                  selectInput('neighborhood', h4('Neighborhood of Interest'), 
                              neighborhoods, selected = 'Airport')),
      conditionalPanel("input.buttons == 'Zipcode'",
                  textInput('zipcode', h4('Zip Code of Interest'), value = "",
                              placeholder = '10007')),
      textInput('search', h4('License Lookup'), value = 'Kidtastix LLC',
                  placeholder = "Kidtastix LLC"),
      fluidRow(textOutput('businessinfo'), align = 'center')
   ),
   dashboardBody(
      tags$head(
         tags$link(rel = "stylesheet", type = 'text/css', href = "custom.css")
      ),
      tabItems(
         #tabItem(tabName = 'questions', fluidPage(box(htmlOutput('motivation',
         #                                                        width = 12)))), 
         tabItem(tabName = 'map', fluidRow(infoBoxOutput("describe", width = 12)),
                  fluidRow(box(leafletOutput('map'), width = 6), 
                           box(plotOutput('boroughs'), width = 6)),
                  fluidRow(conditionalPanel("input.buttons == 'Neighborhood'",
                                            infoBoxOutput('neigh', width = 6)),
                           conditionalPanel("input.buttons == 'Zipcode'",
                                            infoBoxOutput('zipcode', width = 6)),
                           infoBoxOutput('total', width = 6)),
                 fluidRow(conditionalPanel("input.buttons == 'Neighborhood'",
                                           infoBoxOutput('neighnames', width = 12)),
                          conditionalPanel("input.buttons == 'Zipcode'",
                                           infoBoxOutput('zipcodenames', width = 12)))),
         #tabItem(tabName = 'comparisons', fluidRow(plotOutput('comparisons'), 
          #                                         width = 12)),
         tabItem(tabName = 'data', fluidRow(infoBoxOutput('pdf', width = 12)),
                 fluidRow(box(DT::dataTableOutput('table'), 
                                                width = 12)))
         #tabItem(tabName = 'data2', 'income data goes here')
      )
   )
)
)
