#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(shinythemes)
library(plotly)

page_one <- tabPanel(
  "Introduction",
    mainPanel(
      ui <- fluidPage(
        print(uiOutput("intro")),
        theme = shinytheme("cerulean")
      )
    )

)


page_two <- tabPanel(
  "Visualization of Different CO2 Measures by Year and Country",
    sidebarPanel(
      uiOutput("selectColor"),
      uiOutput("selectCountry"),
    ),
    mainPanel(
      ui <- fluidPage(
        plotOutput("barPlot"),
        textOutput("text")
      )
        
    )

)

ui <- navbarPage(
  "CO2 Emissions Across the World",
  page_one,
  page_two
)
