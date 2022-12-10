library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)

CO2_data <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv", stringsAsFactors = FALSE)

#data wrangling


country_CO2_capita_max <- CO2_data %>% 
  select(country, year, co2_per_capita) %>% 
   filter(year == 2021) %>% 
    filter(co2_per_capita == max(co2_per_capita, na.rm = TRUE)) %>% 
      pull(country)
value_CO2_capita_max <- CO2_data %>% 
  select(country, year, co2_per_capita) %>% 
    filter(year == 2021) %>% 
      filter(co2_per_capita == max(co2_per_capita, na.rm = TRUE)) %>% 
        pull(co2_per_capita)

country_CO2_capita_min <- CO2_data %>% 
  select(country, year, co2_per_capita) %>% 
    filter(year == 2021) %>% 
      filter(co2_per_capita == min(co2_per_capita, na.rm = TRUE)) %>% 
        pull(country)
value_CO2_capita_min <- CO2_data %>% 
  select(country, year, co2_per_capita) %>% 
    filter(year == 2021) %>% 
      filter(co2_per_capita == min(co2_per_capita, na.rm = TRUE)) %>% 
        pull(co2_per_capita)

co2_capita_median <- CO2_data %>% 
  select(year, co2_per_capita) %>% 
    filter(year == 2021) %>% 
      summarize(median_co2_per_capita <- median(co2_per_capita, na.rm = TRUE))

variables <-unique(colnames(CO2_data))

introduction_text <-
  paste("## CO2 Emissions Over Time
          
          For this project, I used a dataset that was based off data about CO2
          emissions. This is defined as Annual total production-based emissions of carbon dioxide 
          (CO₂), excluding land-use change, measured in million tonnes. This is
          based on territorial emissions, which do not account for emissions 
          embedded in traded goods. More specifically, I looked at CO2 emissions
          per capita, which is annual total production-based emissions of carbon 
          dioxide (CO₂), excluding land-use change, measured in tonnes per person. 
          This is based on territorial emissions, which do not account for 
          emissions embedded in traded goods. I chose to look at per capita because
          it is a better measure when comparing countries. Per capita takes into 
          account the size of a country's population, while total CO2 emissions
          does not.
          
          ## Important Values
          
          Let's look at some important values before going into our visualization.
          The country with the highest CO2 emissions per capita in 2021 was", 
        formatC(country_CO2_capita_max, format = "d", big.mark = ","), " at ",
        formatC(value_CO2_capita_max, format = "d", big.mark = ","), ". The
          country with the lowest CO2 emissions per capita in 2021 was", 
        formatC(country_CO2_capita_min, format = "d", big.mark = ","), " at ",
        formatC(value_CO2_capita_min, format = "d", big.mark = ","), ". The
          median CO2 emissions per capita in 2021 of all the countries in this 
          dataset was ", formatC(co2_capita_median, format = "d", big.mark = ","), 
        ".")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$selectCountry <- renderUI({
    selectInput("country",
                label = "Select Country to View Data On",
                choices = unique(CO2_data$country),
                selected = "India")
  })
  output$selectColor <- renderUI({
    selectInput("color",
                label = "Select a color",
                choices = c("red", "orange", "green", "blue", "purple"),
                selected = "purple")
  })

  bar_graph <- reactive({
    bar_chart_data <- CO2_data %>% 
      filter(country %in% input$country) 
      #filter(year >= input$years[1] & year <= input$years[2])
  
    ggplot(bar_chart_data, aes(x = year, y = co2_per_capita)) +
    geom_col(aes(fill = input$color)) +
      labs(x = "Years",
           y = "CO2 per capita",
           fill = "Color",
           title = "CO2 Data by Country and Year")
  })
  
  output$barPlot <- renderPlot ({
    bar_graph()
  })
  
  output$text <- renderText({
    paste("The purpose of this bar chart is to demonstrate how CO2 emissions per
          capita have changed over time given a chosen country. In particular, we are looking at 
          annual total production-based emissions of carbon dioxide (CO₂), excluding 
          land-use change, measured in tonnes per person. This is based on territorial emissions,
          which do not account for emissions embedded in traded goods. The user has the
          ability to choose which country they would like to view CO2 per capita for, as well as
          change the color of the barplot for aesthetic purposes. Some clear trends that result
          from this graph are that in general, CO2 per capita has risen over time, starting in 1850
          all the way up until 2021. Additionally, more developed and industrious nation generally have
          higher CO2 emissions per capita than smaller and less developed countries.")
  })
  
  output$intro <- renderUI({
    HTML(markdown::markdownToHTML(text = introduction_text, fragment.only = TRUE))
  })
})

