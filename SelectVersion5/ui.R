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
   dashboardHeader(title = "NYC Businesses"),
   dashboardSidebar(
      
      sidebarUserPanel('Kathryn Bryant',
                       image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
      sidebarMenu(
         menuItem("Single License Exploration", tabName = 'map', icon = icon('globe')),
         menuItem("License Comparisons", tabName = 'comparisons', icon = icon('pie-chart')),
         menuItem('Data', tabName = 'data', icon = icon('table'))
        # menuItem('Income Data', tabName = 'data2', icon = icon('table'))
      ),
      selectInput('selected', 'License Category to View', industries, 
                         selected = NULL),
      numericInput('zipcode', 'Zip Code of Interest', value = 10007)
   ),
   dashboardBody(
      tabItems(
         tabItem(tabName = 'map', fluidRow(box(htmlOutput("describe"), width = 12)),
                  fluidRow(box(leafletOutput('map'), width = 6), 
                           box(plotOutput('boroughs'), width = 6)),
                  fluidRow(infoBoxOutput('total', width = 6), 
                           infoBoxOutput('zipcode', width = 6))),
         tabItem(tabName = 'comparisons', fluidRow(plotlyOutput('comparisons'), 
                                                   width = 12)),
         tabItem(tabName = 'data', fluidRow(box(DT::dataTableOutput('table'), 
                                                width = 12)))
         #tabItem(tabName = 'data2', 'income data goes here')
      )
   )
)
)
