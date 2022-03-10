library(shiny)
library(bslib)
library(shinydashboard)
library(highcharter)
library(tidyverse)
library(plotly)
library(ggplot2)
library(magrittr)
library(readr)
library(shinyjqui)
library(countup)

#loading data


cases_link <-"https://health-infobase.canada.ca/src/data/covidLive/covid19-download.csv"
original_data_cases <- read.csv(url(cases_link)) %>% 
  mutate(date = lubridate::ymd(date)) %>% 
  dplyr::filter(prname != "Repatriated travellers")


fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  # Application title
  titlePanel(
    h1("COVID-19: Past and Present Impact", align = "center")
  ),
  
  h3("The Human Impact of COVID since December 2020", align = "center"),
  
  
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
  
  
  h3("How have vaccines contributed?",  align = "center"),
  
  h4("Effect of Vaccinations on Trends",  align = "center"),
  br(),
  sidebarLayout(
    sidebarPanel(selectInput(inputId = "time_series_data",
                             label = "What kind of statistic are you interested in?", 
                             selected = "Cases",
                             choices = c("Daily Cases", "Daily Death")),
                 selectInput(inputId = "province1",
                             label = "Which province are you interested in? \n (Choose up to 3)", 
                             selected = "Canada", choices = unique(original_data_cases$prname)),
                 selectInput(inputId = "province2",
                             label = NULL, 
                             selected = "None", choices = c('None',unique(original_data_cases$prname))),
                 selectInput(inputId = "province3",
                             label = NULL, 
                             selected = "None", choices = c('None',unique(original_data_cases$prname)))
    ),
    
    mainPanel(plotlyOutput("time_series"))
  ),
  
  br(),
  
  h4("Breakdown of Vaccination Status by Case Type",  align = "center"),
  br(),
  
  verticalLayout(
    column(width=12,checkboxGroupInput( "casetype",
                                        "Case Type (Check all applicable):",
                                        choices= c("Cases", "Hospiatlization", "Death"),
                                        inline = T,
                                        selected = "Cases"),
           align = "center"),
    
    column(width=8, offset = 2, plotlyOutput("cases_and_vaccines")),
    
  ),
  
  br(),
  
  
  
)