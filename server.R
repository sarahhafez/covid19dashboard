cases_link <-"https://health-infobase.canada.ca/src/data/covidLive/covid19-download.csv"
original_data_cases <- read.csv(url(cases_link)) %>% 
  mutate(date = lubridate::ymd(date))


cases_and_vaccine_link <- "https://health-infobase.canada.ca/src/data/covidLive/covid19-epiSummary-casesAfterVaccination.csv"
cases_and_vaccine_data <- read.csv(url(cases_and_vaccine_link)) %>%
  select(-c("label.fr","prop_cases","prop_hospitalizations","prop_deaths"))  %>%
  rename(Cases = num_cases,
         Hospiatlization = num_hospitalizations,
         Death = num_deaths) %>%
  pivot_longer(cols=c("Cases","Hospiatlization","Death"),
               names_to = "case_type" )



# define function

createdata <- function(province1,province2,province3) {
  if (province2 == "None" & province3 == "None") {
    filter_data <- original_data_cases %>% 
      dplyr::filter(prname == province1) 
    return(filter_data)
  } else if (province2 == "None" & province3 != "None") {
    filter_data <- original_data_cases %>% 
      dplyr::filter(prname %in% c(province1,province3))  
    return(filter_data)
  } else if (province2 != "None" & province3 == "None") {
    filter_data <- original_data_cases %>% 
      filter(prname %in% c(province1,province2)) 
    return(filter_data)
  } else if (province2 != "None" & province3 != "None") {
    filter_data <- original_data_cases %>% 
      dplyr::filter(prname %in% c(province1,province2,province3))
    return(filter_data)
  }
}


function(input, output) {
  
  canada_latest_covid_numbers <- original_data_cases %>%
    arrange(date) %>%
    dplyr::filter(prname == "Canada") %>%
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
      dplyr::filter(prname!="Repatriated travellers" && prname!="Canada") %>%
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
  
  
  output$time_series <- renderPlotly({
    
    if (input$time_series_data == "Daily Cases") {
      value = "numtoday"
      ymax=max(createdata(input$province1,input$province2,input$province3)$numtoday)
      t = "Number of Reported Cases per Day"
      
    } else {
      value = "numdeathstoday"
      ymax=max(createdata(input$province1,input$province2,input$province3)$numdeathstoday)
      t = "Number of Reported Deaths per Day"
    }
    
    
    plot <- createdata(input$province1,input$province2,input$province3) %>% 
      ggplot(aes_string(x="date", y=value,col="prname")) +
      geom_line() + 
      labs(title = t,
           x = "Date",
           y = "Number of Reported Cases",
           color="Province") +
      geom_vline(xintercept=as.numeric(lubridate::ymd("2020-12-14")), colour="grey", linetype="dotted")+
      geom_text(aes(x=lubridate::ymd("2020-12-14"), y=ymax, 
               label="First Dose"), colour="grey")+
      geom_vline(xintercept=as.numeric(lubridate::ymd("2021-05-25")), colour="grey" , linetype="dotted")+
      geom_text(aes(x=lubridate::ymd("2021-05-25"), y=ymax, 
                    label="Second Dose"), colour="grey")+
      geom_vline(xintercept=as.numeric(lubridate::ymd("2021-11-12")), colour="grey", linetype="dotted")+
      geom_text(aes(x=lubridate::ymd("2021-11-12"), y=ymax, 
                    label="Third Dose"), colour="grey")+
      scale_color_manual(values = c("#4E84C4", "#E7B800", "#FC4E07"))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    our_plotly_plot <- ggplotly(plot)
    
    return(our_plotly_plot)
    
    
    
  }) 
  
  output$cases_and_vaccines <- renderPlotly({
    plot <- cases_and_vaccine_data %>%
      dplyr::filter(case_type %in% input$casetype) %>%
      ggplot(aes(x=case_type, y=value, fill = label.en)) +
      geom_bar(position = "fill", stat = "identity") 
    
    return(plot)
    
  })
  
  
  output$cases_and_vaccines <- renderPlotly({
    plot <- cases_and_vaccine_data %>%
      filter(case_type %in% input$casetype) %>%
      ggplot(aes(x=case_type, y=value, fill = label.en), label = scales::percent(value)) +
      geom_bar( stat = "identity", position = "fill", width=0.3)  +
      theme_classic() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      scale_fill_manual(values = c("#293352", "#4E84C4",  "#E7B800", "#F4EDCA","#D16103" )) +
      labs(fill = "Vaccination Status",
           x = "Case Type",
           y="Percentage Breakdown",
           title = "Breakdown of Cases by Vaccination Status") 
    
    plot
    
    
  })

}

