library(shiny)
library(highcharter)
library(shinydashboard)
library(tidyverse)
library(plotly)

cases_link <-"https://health-infobase.canada.ca/src/data/covidLive/covid19-download.csv"
original_data_cases <- read.csv(url(cases_link)) %>% 
  mutate(date = lubridate::ymd(date))


fluidPage(
  # Application title
  titlePanel("COVID-19: Past and Present Impact"),
  
  h2("The Human Impact of COVID since December 2020"),
  
  
  #First start with the Map of Canada, showing the total deaths and cases in each provinces
  #On the side there is a statistic that shows
  
  
  fluidRow(
    column(8,
           highchartOutput("canadianMap")),
    
    column(4,
           
           fixedRow(valueBox(uiOutput("totalcases"), "Total Cases")),
           fixedRow(valueBox(uiOutput("totaldeaths"), "Total Deaths"))
    )
  ), 
  
  
  br(),
  
  h2("How have vaccines helped?"),
  
  sidebarLayout(
    sidebarPanel(selectInput(inputId = "time_series_data",
                             label = "Wat kind of number are you interested in?", 
                             selected = "Cases", choices = c("Daily Cases", "Daily Death", "Daily Active")),
                 selectInput(inputId = "province1",
                             label = "Which province are you interested in?", 
                             selected = "Canada", choices = unique(original_data_cases$prname)),
                 selectInput(inputId = "province2",
                             label = "Which province are you interested in?", 
                             selected = "None", choices = c('None',unique(original_data_cases$prname))),
                 selectInput(inputId = "province3",
                             label = "Which province are you interested in?", 
                             selected = "None", choices = c('None',unique(original_data_cases$prname)))
    ),
    
    mainPanel(plotlyOutput("time_series"))
  ),
  
  
  
  
  
  h1("Vaccination Acceptance"),
  
  verticalLayout(
    highchartOutput("pieChart"),
    radioButtons("gender",
                 "Gender",
                 choiceNames = c("Male", "Female", "All"),
                 choiceValues = c("male", "female", "all")),
    
  )
  
  
)