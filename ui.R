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

canada_latest_covid_numbers <- original_data_cases %>%
  arrange(date) %>%
  dplyr::filter(prname == "Canada") %>%
  tail(n=1)

exposure_link <- "https://health-infobase.canada.ca/src/data/covidLive/covid19-epiSummary-probableexposure2.csv"
exposure_data <- read.csv(url(exposure_link))  %>%
  select(c("Probable.exposure.setting", "Number_of_cases" )) %>%
  rename(setting = Probable.exposure.setting) %>%
  filter(setting != "Cases with probable exposure data" & setting != "Information pending")

known_exposure <- exposure_data %>% filter(setting == "Spread")  %>% .[["Number_of_cases"]]
total <- canada_latest_covid_numbers$numtotal

fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  # Application title
  titlePanel(
    h1("COVID-19 in Canada: A Summary", align = "center"),
    windowTitle = "COVID-19 in Canada: A Summary"
  ),
  
  h3("The Human Impact of COVID-19 since December 2019", align = "center"),
  
  
  
  #First start by showing some statistics
  
  fluidRow(
    
    #align="center",
    
    column(3,valueBox(countupOutput("totalcases"), "Total Cases")),
    column(3,valueBox(countupOutput("totaldeaths"), "Total Deaths")),
    column(3,valueBox(countupOutput("deathsLastWeek"), "Deaths Last 7 Days")),
    column(3,valueBox(countupOutput("numberOfActiveCases"), "Active Cases Today"))
    
  ),
  
  br(),
  
  #Secondly, a Map of Canada, showing the total deaths and cases in each provinces
  
  
  fluidRow(highchartOutput("canadianMap")),
  
  
  br(),
  
  #Thirdly, a time series chart showing the trends for either daily deaths/cases in each province
  
  
  h3("COVID-19 Trends By Province",  align = "center"),
  br(),
  sidebarLayout(
    sidebarPanel(selectInput(inputId = "time_series_data",
                             label = "What kind of statistic are you interested in?", 
                             selected = "Cases",
                             choices = c("Daily Cases", "Daily Deaths")),
                 selectInput(inputId = "province1",
                             label = "Which provinces are you interested in? \n (Choose up to 3)", 
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
  
  #Thirdly, a time series chart showing the trends for either daily deaths/cases in each province
  
  h3("Sources of COVID-19 Spread and Exposure",  align = "center"),
  
  paste("We only know the probable exposure of", 
        format(known_exposure, big.mark=","),
        "cases, representing approximately 47% of the total number of cases.") %>%
    p(align = "center"),
  
  #Fourthly, a pie chart showing the probable causes of spread/exposure
  fluidRow(highchartOutput("exposureChart")),
  
  br(),
  
  h4("How have vaccines contributed?",  align = "center"),
  
  #Finally, a stacked bar chart showing the vaccination status by case type
  h4("Breakdown of Vaccination Status by Case Type and Outcome",  align = "center"),
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