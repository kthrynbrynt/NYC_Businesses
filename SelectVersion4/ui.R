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
   dashboardHeader(title = "NYC Businesses"),
   dashboardSidebar(
      
      sidebarUserPanel('Kathryn Bryant',
                       image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
      sidebarMenu(
         menuItem("Map", tabName = 'map', icon = icon('globe')),
         #menuItem("Borough Comparisons", tabName = 'boroughs', icon = icon('bar-chart')),
         menuItem('Business Data', tabName = 'data', icon = icon('table'))
        # menuItem('Income Data', tabName = 'data2', icon = icon('table'))
      ),
      selectInput('selected', 'Industry to View', industries, 
                         selected = NULL)
      #checkboxInput('typecheck', 'Show Business Type', types)
   ),
   dashboardBody(
      tabItems(
         tabItem(tabName = 'map', fluidRow(box(htmlOutput("describe"), width = 12)),
                  fluidRow(box(leafletOutput('map'), width = 6), 
                           box(plotOutput('boroughs'), width = 6)),
                  fluidRow(infoBoxOutput("total", width = 5))),
         #tabItem(tabName = 'boroughs', plotOutput('boroughs')),
         tabItem(tabName = 'data', fluidRow(box(DT::dataTableOutput('table'))))
         #tabItem(tabName = 'data2', 'income data goes here')
      )
   )
)
)
