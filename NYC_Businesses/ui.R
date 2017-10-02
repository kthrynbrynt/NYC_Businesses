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
         menuItem('Business Data', tabName = 'data1', icon = icon('table')),
         menuItem('Income Data', tabName = 'data2', icon = icon('table'))
      ),
      checkboxGroupInput('checked', 'Industries to Plot', industries, 
                         selected = NULL)
   ),
   dashboardBody(
      tabItems(
         tabItem(tabName = 'map', leafletOutput('map')),
         tabItem(tabName = 'data1', 'business data goes here'),
         tabItem(tabName = 'data2', 'income data goes here')
      )
   )
)
)
