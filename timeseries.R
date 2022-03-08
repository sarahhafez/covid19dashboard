




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

# define function

createdata <- function(province1,province2,province3) {
    if (province2 == "None" & province3 == "None") {
        filter_data <- df %>% 
            filter(prname == province1) 
        return(filter_data)
    } else if (province2 == "None" & province3 != "None") {
        filter_data <- df %>% 
            filter(prname %in% c(province1,province3))  
        return(filter_data)
    } else if (province2 != "None" & province3 == "None") {
        filter_data <- df %>% 
            filter(prname %in% c(province1,province2)) 
        return(filter_data)
    } else if (province2 != "None" & province3 != "None") {
        filter_data <- df %>% 
            filter(prname %in% c(province1,province2,province3))
        return(filter_data)
    }
}


# Define UI 
ui <- fluidPage(
    theme = bs_theme(bootswatch = "minty"),
    
    titlePanel('COVID Cases Canada'),
    
    
    sidebarLayout(
        sidebarPanel(selectInput(inputId = "time_series_data",
                                 label = "Wat kind of number are you interested in?", 
                                 selected = "Daily Cases", choices = c("Daily Cases", "Daily Death")),
                     selectInput(inputId = "province1",
                                 label = "Which province are you interested in?", 
                                 selected = "Canada", choices = unique(df$prname)),
                     selectInput(inputId = "province2",
                                 label = "Which province are you interested in?", 
                                 selected = "None", choices = c('None',unique(df$prname))),
                     selectInput(inputId = "province3",
                                 label = "Which province are you interested in?", 
                                 selected = "None", choices = c('None',unique(df$prname)))
                     ),
        
        mainPanel(plotlyOutput("time_series"))
    )
)

# Define server logic
server <- function(input, output) {
    
    output$time_series <- renderPlotly( {
    
        if (input$time_series_data == "Daily Cases") {
            our_plot <- createdata(input$province1,input$province2,input$province3) %>% 
                ggplot(aes(date, numtoday,col=prname)) +
                geom_line() + 
                labs(title = "Number of Reported Cases per day",
                     x = "Date",
                     y = "Number of Reported Cases",
                     color="Province name")
            ymax=max(createdata(input$province1,input$province2,input$province3)$numtoday)
            plot <- our_plot +
                geom_vline(xintercept=as.numeric(lubridate::ymd("2020-12-14")))+
                annotate('text', x=lubridate::ymd("2020-12-14"), y=ymax,
                         label="\nFirst dose starts",angle=45)+
                geom_vline(xintercept=as.numeric(lubridate::ymd("2021-05-25")))+
                annotate("text", x=lubridate::ymd("2021-05-25"), y=ymax, 
                         label="\nSecond dose starts")+
                geom_vline(xintercept=as.numeric(lubridate::ymd("2021-11-12")))+
                annotate("text", x=lubridate::ymd("2021-11-12"), y=ymax, 
                         label="\nThird dose starts")+
                theme_classic()
            
            our_plotly_plot <- ggplotly(plot)
            
            return(our_plotly_plot)
        } else if (input$time_series_data == "Daily Death") {
            our_plot <- createdata(input$province1,input$province2,input$province3) %>%
                ggplot(aes(date, numdeathstoday,col=prname)) +
                geom_line() + 
                labs(title = "Number of Death per day",
                     x = "Date",
                     y = "Number of Death",
                     color="Province name")
            ymax=max(createdata(input$province1,input$province2,input$province3)$numdeathstoday)
            plot <- our_plot +
                geom_vline(xintercept=as.numeric(lubridate::ymd("2020-12-14")))+
                annotate("text", x=lubridate::ymd("2020-12-14"), y=ymax, 
                         label="\nFirst dose starts")+
                geom_vline(xintercept=as.numeric(lubridate::ymd("2021-05-25")))+
                annotate("text", x=lubridate::ymd("2021-05-25"), y=ymax, 
                         label="\nSecond dose starts")+
                geom_vline(xintercept=as.numeric(lubridate::ymd("2021-11-12")))+
                annotate("text", x=lubridate::ymd("2021-11-12"), y=ymax, 
                         label="\nThird dose starts")+
                theme_classic()
            
            our_plotly_plot <- ggplotly(plot)
            
            return(our_plotly_plot)
        } 
        
    }) 
    
}

# Run the application 
shinyApp(ui = ui, server = server)
