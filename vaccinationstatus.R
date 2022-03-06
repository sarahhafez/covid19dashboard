library(shiny)
library(tidyverse)
library(magrittr)
library(highcharter) 
#get data 

link <-"https://health-infobase.canada.ca/src/data/covidLive/covid19-epiSummary-casesAfterVaccinationGender.csv"
df <- read.csv(url(link)) %>% select(-"label.fr")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID Cases by Vaccination Status and Gender"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("gender",
                        "Gender:",
                        choice = c("male", "female", "total"))
                       
        ),

        # Show a plot of the generated distribution
        mainPanel(
            highchartOutput("pieChart")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$pieChart <-  renderHighchart({
        df %<>% filter(gender == input$gender) %>% select(-"gender")
        df %>%
            hchart("pie", hcaes(x = "label.en", y = "num_cases"),
            name = "Number of Cases")

    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
