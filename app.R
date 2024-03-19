#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
rm(list = ls())
library(shiny)
library(reactablefmtr)
library(sf)
library(readr)
library(dplyr)
library(lubridate)
library(plotly)
library(stringr)
library(purrr)
library(ggplot2)
library(bslib)


#Source all files of additional functions
list.files("R/", pattern = "*.R", all.files = T, full.names = T) %>% 
  walk(., source)

#Get the time of the last update of the data
last_updated_date = as.Date(file.info("data.csv")$ctime)

mozmap = preprocess_map()

#Read the data in a robust way
all_data = read_csv("data.csv", 
                    col_types = cols(
                       Region  = col_character(),
                       disease = col_character(),
                       .default = col_double(),
                       date = col_date(format = "%Y-%m-%d"),
                       type = col_character()
                      )) %>% 
  filter(year(date) > 2018) 

number_of_predicted_dates = all_data %>% 
  group_by(type) %>% 
  summarise(dmax = max(date)) %>% 
  ungroup() %>% 
  mutate(dmax = as.numeric(difftime(dmax, lag(dmax), units = "weeks"))) %>% 
  filter(!is.na(dmax)) %>% 
  mutate(dmax = abs(dmax)) %>% 
  pull(dmax)

summary_data = all_data %>% 
  filter(date  > max(date) - weeks(2*number_of_predicted_dates)) %>% 
  group_by(Region, type, disease) %>% 
  summarise(
    incident_cases = sum(incident_cases), 
    min_date       = min(date),
    max_date       = max(date),
    .groups = "drop") %>% 
  arrange(type) %>% 
  group_by(Region, disease) %>% 
  mutate(trend = (incident_cases - lag(incident_cases))/lag(incident_cases)) %>% 
  ungroup() %>% 
  filter(!is.na(trend))  %>% 
  select(-type) %>% 
  mutate(period = paste0(format(min_date,"%b %d"), " a " ,format(max_date,"%b %d"))) %>% 
  select(-min_date, -max_date)



#Calculate the percent change in 

# Define UI for application that draws a histogram
ui <- fluidPage(theme = bs_theme(preset = "sandstone", version = 5),

    # Application title
    titlePanel("MÁQUINA: Modelo de Análise Quantitativa para Doenças Infecciosas"),

    mainPanel(
      tabsetPanel(
        id = "tabset",
        get_disease_panel("Malária", "malaria"),
        get_disease_panel("Doenças diarréicas","diarrhea"),
 
        
        tabPanel("Sobre", 
                 h2("Brief explanation of the model here"),
                 p("bla bla bla"),
                 value = "about"),
        
      ),
      width = 12
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$malaria_table <- renderReactable({
    table_trend_cases(summary_data, disease_name = "Malaria")
  })
  
  output$diarrhea_table <- renderReactable({
    table_trend_cases(summary_data, disease_name = "Diarrhea")
  })
  
  output$malaria_map_past <- renderPlotly({
    plot_map(summary_data, mapdata = mozmap, disease_name = "Malaria",
             title = "Casos pasados")
  })
  
  output$malaria_map_future <- renderPlotly({
    plot_map(summary_data, mapdata = mozmap, disease_name = "Malaria",
             title = "Casos previstos")
  })
  
  output$diarrhea_map <- renderPlotly({
    plot_map(summary_data, mapdata = mozmap, disease_name = "Diarrhea")
  })
  
  output$malaria_plot <- renderPlot({
    plot_forecast(all_data, last_updated_date = last_updated_date, 
                  disease_name = "Malaria")
  },
  res = 100)
  
  output$diarrhea_plot <- renderPlot({
    plot_forecast(all_data, last_updated_date = last_updated_date,
                  disease_name = "Diarrhea")
  },
  res = 100)
}

# Run the application 
shinyApp(ui = ui, server = server)
