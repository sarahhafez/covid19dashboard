cases_link <-"https://health-infobase.canada.ca/src/data/covidLive/covid19-download.csv"
original_data_cases <- read.csv(url(cases_link)) %>% 
  mutate(date = lubridate::ymd(date))

# define function

createdata <- function(province1,province2,province3) {
  if (province2 == "None" & province3 == "None") {
    filter_data <- original_data_cases %>% 
      filter(prname == province1) 
    return(filter_data)
  } else if (province2 == "None" & province3 != "None") {
    filter_data <- original_data_cases %>% 
      filter(prname %in% c(province1,province3))  
    return(filter_data)
  } else if (province2 != "None" & province3 == "None") {
    filter_data <- original_data_cases %>% 
      filter(prname %in% c(province1,province2)) 
    return(filter_data)
  } else if (province2 != "None" & province3 != "None") {
    filter_data <- original_data_cases %>% 
      filter(prname %in% c(province1,province2,province3))
    return(filter_data)
  }
}


function(input, output) {
  
  canada_latest_covid_numbers <- original_data_cases %>%
    arrange(date) %>%
    filter(prname == "Canada") %>%
    tail(n=1)
  
  output$totalcases <- renderCountup({ 
    countup(canada_latest_covid_numbers$numtotal,
            duration = 2)
  })
  
  
  output$totaldeaths <- renderCountup({ 
    countup(canada_latest_covid_numbers$numdeaths,
            duration = 2)
  })
  
  output$deathsLastSevenDays <- renderCountup({ 
    countup(canada_latest_covid_numbers$numdeaths_last7,
            duration = 2)
  })
  
  output$numberOfActiveCases <- renderCountup({ 
    countup(canada_latest_covid_numbers$numactive,
            duration = 2)
  })
  
  output$canadianMap <- renderHighchart({
    
    
    df <- original_data_cases %>%
      select(c("prname", "numdeathstoday", "numtoday")) %>%
      group_by(prname) %>%
      summarise("Total Deaths" = sum(numdeathstoday), "Total Cases" = sum(numtoday))  %>%
      filter(prname!="Repatriated travellers" && prname!="Canada") %>%
      mutate(prname = replace(prname, prname == "Quebec", "Québec")) #needed for the map
    
    
    hcmap('countries/ca/ca-all',
          data = df,
          value = "Total Cases",
          name =  "Total Cases",
          joinBy = c('woe-name', 'prname'),
          dataLabels = list(enabled = TRUE, format = "{point.name}"),
          legend = F,
          nullColor = "#DADADA",
          showInLegend = F) %>%
      hc_title(text = "Total Cases by Province") %>%
      hc_subtitle(text = "Ontario and Quebec have recorded the highest number of cases") %>%
      hc_legend(enabled = F)
      
    
  })
  
  
  output$time_series <- renderPlotly( {
    
    if (input$time_series_data == "Daily Cases") {
      our_plot <- createdata(input$province1,input$province2,input$province3) %>% 
        ggplot(aes(date, numtoday,col=prname)) +
        geom_line() + 
        labs(title = "Number of Reported Cases per day",
             x = "Date",
             y = "Number of Reported Cases",
             color="Province name")
      plot <- our_plot +
        geom_vline(xintercept=as.numeric(lubridate::ymd("2020-12-14")))+
        annotate("text", x=lubridate::ymd("2020-12-14"), y=0, 
                 label="\nFirst dose starts")+
        geom_vline(xintercept=as.numeric(lubridate::ymd("2021-05-25")))+
        annotate("text", x=lubridate::ymd("2021-05-25"), y=0, 
                 label="\nSecond dose starts")+
        geom_vline(xintercept=as.numeric(lubridate::ymd("2021-11-12")))+
        annotate("text", x=lubridate::ymd("2021-11-12"), y=0, 
                 label="\nThird dose starts")
      
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
      
      plot <- our_plot +
        geom_vline(xintercept=as.numeric(lubridate::ymd("2020-12-14")))+
        annotate("text", x=lubridate::ymd("2020-12-14"), y=0, 
                 label="\nFirst dose starts")+
        geom_vline(xintercept=as.numeric(lubridate::ymd("2021-05-25")))+
        annotate("text", x=lubridate::ymd("2021-05-25"), y=0, 
                 label="\nSecond dose starts")+
        geom_vline(xintercept=as.numeric(lubridate::ymd("2021-11-12")))+
        annotate("text", x=lubridate::ymd("2021-11-12"), y=0, 
                 label="\nThird dose starts")
      
      our_plotly_plot <- ggplotly(plot)
      
      return(our_plotly_plot)
    } else if (input$time_series_data == "Daily Active") {
      our_plot <- createdata(input$province1,input$province2,input$province3) %>%
        ggplot(aes(date, numactive,col=prname)) +
        geom_line() + 
        labs(title = "Number of Active Cases per day",
             x = "Date",
             y = "Number of Active Cases",
             color="Province name")
      
      plot <- our_plot +
        geom_vline(xintercept=as.numeric(lubridate::ymd("2020-12-14")))+
        annotate("text", x=lubridate::ymd("2020-12-14"), y=0, 
                 label="\nFirst dose starts")+
        geom_vline(xintercept=as.numeric(lubridate::ymd("2021-05-25")))+
        annotate("text", x=lubridate::ymd("2021-05-25"), y=0, 
                 label="\nSecond dose starts")+
        geom_vline(xintercept=as.numeric(lubridate::ymd("2021-11-12")))+
        annotate("text", x=lubridate::ymd("2021-11-12"), y=0, 
                 label="\nThird dose starts")
      
      our_plotly_plot <- ggplotly(plot)
      
      return(our_plotly_plot)
    }
    
  }) 
  
  
  output$pieChart <-  renderHighchart({
    link <-"https://health-infobase.canada.ca/src/data/covidLive/covid19-epiSummary-casesAfterVaccinationGender.csv"
    df <- read.csv(url(link)) %>% select(-"label.fr")
    
    df %<>% filter(gender == input$gender) %>% select(-"gender")
    df %>%
      hchart("pie", hcaes(x = "label.en", y = "num_cases"),
             name = "Number of Cases")
    
  })
}

