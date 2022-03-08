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

cases_link <-"https://health-infobase.canada.ca/src/data/covidLive/covid19-download.csv"
original_data_cases <- read.csv(url(cases_link)) %>% 
  mutate(date = lubridate::ymd(date))


fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  # Application title
  titlePanel(
    h1("COVID-19: Past and Present Impact", align = "center")
    ),
  
  h2("The Human Impact of COVID since December 2020", align = "center"),
  
  
  #First start with the Map of Canada, showing the total deaths and cases in each provinces
  #On the side there is a statistic that shows the total deaths and cases in Canada
  
  
  fluidRow(
    
    align="center",
    
    column(3,valueBox(countupOutput("totalcases"), "Total Cases")),
    column(3,valueBox(countupOutput("totaldeaths"), "Total Deaths")),
    column(3,valueBox(countupOutput("deathsLastSevenDays"), "Deaths Last Week")),
    column(3,valueBox(countupOutput("numberOfActiveCases"), "Active Cases"))
    
  ),
  
  br(),
  
  
  
  fluidRow(highchartOutput("canadianMap")),
  
  
  br(),

  
  h2("How have vaccines contributed?",  align = "center"),
  br(),
  
  h3("Effect of Vaccinations on Trends",  align = "center"),
  
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
  
  
  
  
  
  h3("Breakdown of Vaccination Status by Cases Status and Gender",  align = "center"),
  
  verticalLayout(
    highchartOutput("pieChart"),
    radioButtons("gender",
                 "Gender",
                 choiceNames = c("Male", "Female", "All"),
                 choiceValues = c("male", "female", "all")),
    
  )
  
  
)