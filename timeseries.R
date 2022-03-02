




# load the libraries ------------------------------------------------------
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(bslib)


# load our data -----------------------------------------------------------

link <- "https://health-infobase.canada.ca/src/data/covidLive/covid19-download.csv"
df <- read.csv(url(link))
df <- df %>% 
    mutate(date = lubridate::ymd(date))


# Define UI 
ui <- fluidPage(
    theme = bs_theme(bootswatch = "minty"),
    
    titlePanel('COVID Cases Canada'),
    
    
    sidebarLayout(
        sidebarPanel(selectInput(inputId = "time_series_data",
                                 label = "Which number are you interested in?", 
                                 selected = "Cases", choices = c("Cases", "Death", "Active")),
                     selectInput(inputId = "province",
                                 label = "Which number are you interested in?", 
                                 selected = "Canada", choices = unique(df$prname))
                     ),
        
        mainPanel(plotlyOutput("time_series"))
    )
)

# Define server logic
server <- function(input, output) {
    
    output$time_series <- renderPlotly( {
        
        filter_data <- df %>% 
            filter(prname == input$province)
        
        if(input$time_series_data == "Cases") {
            our_plot <- filter_data %>% 
                ggplot(aes(date, numtoday)) +
                geom_line() + 
                labs(title = "Number of Reported Cases per day",
                     x = "Date",
                     y = "Number of Cases")
            
            our_plotly_plot <- ggplotly(our_plot)
            
            return(our_plotly_plot)
        } else if (input$time_series_data == "Death") {
            our_plot <- filter_data %>% 
                ggplot(aes(date, numdeathstoday)) +
                geom_line() + 
                labs(title = "Number of Death per day",
                     x = "Date",
                     y = "Number of Death")
            
            our_plotly_plot <- ggplotly(our_plot)
            
            return(our_plotly_plot)
        } else if (input$time_series_data == "Active") {
            our_plot <- filter_data %>% 
                ggplot(aes(date, numactive)) +
                geom_line() + 
                labs(title = "Number of Active Cases per day",
                     x = "Date",
                     y = "Number of Active Cases")
            
            our_plotly_plot <- ggplotly(our_plot)
            
            return(our_plotly_plot)
        }
        
    }) 
    
}

# Run the application 
shinyApp(ui = ui, server = server)
