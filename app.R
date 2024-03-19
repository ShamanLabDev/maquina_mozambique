#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(reactablefmtr)
library(patchwork)
library(sf)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(purrr)

fles = list.files("R/", pattern = "*.R", all.files = T, full.names = T)
walk(fles, source)

last_updated_date = as.Date(file.info("data.csv")$ctime)

mozmap = read_sf("mapfiles/moz_admbnda_adm1_ine_20190607.shp") %>% 
  mutate(Region = case_when(
    str_detect(ADM1_PT,"City") ~ "Maputo (cidade)",
    str_detect(ADM1_PT,"Zambez") ~ "Zambézia",
    .default = ADM1_PT
  ))

#Read the data in a robust way
malaria_data = read_csv("data.csv", 
                         col_types = cols(
                         EpiWeek = col_integer(),
                         Region  = col_character(),
                         Malaria_Cases = col_double(),
                         low = col_double(),
                         upp = col_double(),
                         date = col_date(format = "%Y-%m-%d"),
                         type = col_character()
                         )) %>% 
  filter(year(date) > 2018) 

number_of_predicted_dates = malaria_data %>% 
  group_by(type) %>% 
  summarise(dmax = max(date)) %>% 
  ungroup() %>% 
  mutate(dmax = as.numeric(difftime(dmax, lag(dmax), units = "weeks"))) %>% 
  filter(!is.na(dmax)) %>% 
  mutate(dmax = abs(dmax)) %>% 
  pull(dmax)

summary_data_malaria = malaria_data %>% 
  filter(date  > max(date) - weeks(2*number_of_predicted_dates)) %>% 
  group_by(Region, type) %>% 
  summarise(Malaria_Cases = sum(Malaria_Cases), .groups = "drop") %>% 
  arrange(type) %>% 
  group_by(Region) %>% 
  mutate(trend = (Malaria_Cases - lag(Malaria_Cases))/lag(Malaria_Cases)) %>% 
  ungroup() %>% 
  filter(!is.na(trend))  %>% 
  select(-type) %>% 
  mutate(last_update = !!last_updated_date)

summary_data_diarrhea = summary_data_malaria

mapdata = summary_data_malaria %>% 
  left_join(mozmap, by = join_by(Region))


#Calculate the percent change in 

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MÁQUINA: Modelo de Análise Quantitativa para Doenças Infecciosas"),

    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel("Malária", 
                 h2("Malária"),
                 p("This text is automatic from the data:
                   A total of 1,000 new cases are expected in the next two weeks
                   with Cabo Delgado (1,181), Maputo (1,539), and Gaza (1,539) 
                   seeing the largest increase. The trend is currently decreasing 
                   with the highest decrements happening in Nampula, Maputo, and Sofala."),
                 fluidRow(
                   column(12, align="center",
                      plotOutput("malaria_map", height = "600px", width = "100%")
                   )
                 ),
                 fluidRow(
                   column(12, align="center",
                      reactableOutput("malaria_table", height = "500px", width = "100%"),
                   )
                 ),
                 p("This is a different paragraph that also contains numbers generated from the data"),
                 fluidRow(
                   column(12, align="center",
                      plotOutput("malaria_plot", height = "600px", width = "100%")
                   )
                 ),                
                 p("Please remember that this dashboard's projections are generated 
                   by an automated model and may contain errors. Understanding 
                 epidemiological concepts is essential for accurate 
                 interpretation. Consult additional sources for context."),
                 fluidRow(
                   column(6, align="center", offset = 3,
                     downloadButton(
                       "download_malaria",
                       label = "Download PDF",
                       class = NULL,
                       icon = shiny::icon("download")
                     )
                    )
                 ),
                 value = "malaria"),
        
        tabPanel("Doenças diarréicas", 
                 h2("Doenças diarréicas"),
                 p("This is the same as the malaria one but with diarrhea"),
                 fluidRow(
                   column(12, align="center",
                      plotOutput("diarrhea_map", height = "600px", width = "100%")
                   )
                 ),
                 fluidRow(
                   column(12, align="center",
                     reactableOutput("diarrhea_table", height = "500px", width = "100%"),
                   )
                 ),
                 p("something something"),
                 fluidRow(
                   column(12, align="center",
                     plotOutput("diarrhea_plot", height = "600px", width = "100%")
                   )
                 ),
                 p("Please remember that this dashboard's projections are generated 
                   by an automated model and may contain errors. Understanding 
                 epidemiological concepts is essential for accurate 
                 interpretation. Consult additional sources for context."),
                 fluidRow(
                   column(6, align="center", offset = 3,
                          downloadButton(
                            "download_diahrrea",
                            label = "Download PDF",
                            class = NULL,
                            icon = shiny::icon("download")
                          )
                   )
                 ),
                 value = "diarrhea"),
        
        tabPanel("Sobre", 
                 h2("Brief explanation of the model here"),
                 p("bla bla bla"),
                 h2("Comparison of previous predictions"),
                 p("The following plots show the previous predictions vs
                   what actually happened as a way to show the model's performance"),
                 fluidRow(
                   column(12, align="center",
                          plotOutput("predict_plot", height = "600px", 
                                     width = "100%")
                   )
                 ),
                 value = "about"),
        
      ),
      width = 12
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$malaria_table <- renderReactable({
    table_trend_cases(summary_data_malaria)
  })
  
  output$diarrhea_table <- renderReactable({
    table_trend_cases(summary_data_diarrhea)
  })
  
  output$malaria_map <- renderPlot({
    plot_map(mapdata)
  },
  res = 100)
  
  output$diarrhea_map <- renderPlot({
    plot_map(mapdata)
  },
  res = 100)
  
  output$predict_plot = renderPlot({
    plot_comparison(malaria_data, malaria_data, last_updated_date = last_updated_date)
  },
  res = 100)
  
  output$malaria_plot <- renderPlot({
    plot_forecast(malaria_data, last_updated_date = last_updated_date)
  },
  res = 100)
  
  output$diarrhea_plot <- renderPlot({
    plot_forecast(malaria_data, last_updated_date = last_updated_date)
  },
  res = 100)
}

# Run the application 
shinyApp(ui = ui, server = server)
