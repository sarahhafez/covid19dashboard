library(shiny)
library(bslib)
library(shinydashboard)
library(highcharter)
library(plotly)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(readr)
library(shinyjqui)
library(countup)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    fluidRow(highchartOutput("canadianMap"))
  
)

# Define server logic 
server <- function(input, output) {
    
    output$canadianMap <- renderHighchart({
        link <-"https://health-infobase.canada.ca/src/data/covidLive/covid19.csv"
        original_data <- read.csv(url(link))
        
        df <- original_data %>%
            select(c("prname", "numdeathstoday", "numtoday")) %>%
            group_by(prname) %>%
            summarise("Total Deaths" = sum(numdeathstoday), "Total Cases" = sum(numtoday))  %>%
            filter(prname!="Repatriated travellers" && prname!="Canada") %>%
            mutate(prname = replace(prname, prname == "Quebec", "Québec")) #needed for the map
        
        hcmap('countries/ca/ca-all',
              data = df,
              value = "Total Cases",
              joinBy = c('woe-name', 'prname'),
              dataLabels = list(enabled = TRUE, format = "{point.name}"),
              showInLegend = F)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
