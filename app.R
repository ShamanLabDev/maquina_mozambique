#Libraries-----
rm(list = ls())
library(shiny)
library(reactablefmtr)
library(sf)
library(readr)
library(dplyr)
library(lubridate)
library(plotly)
library(purrr)
library(ggplot2)
library(bslib)
library(wesanderson)
library(tidyr)

#Preprocessing----
list.files("R/", pattern = "*.R", all.files = T, full.names = T) %>% 
  walk(., source)

#Get the shapefiles for the map
mozmap = preprocess_map()

#Read the data in a robust way
all_data = get_data()
regions  = sort(unique(all_data$Region))


# User inferface----
ui <- navbarPage(
  
  #Theme----
  theme = bs_theme(preset = "sandstone", version = 5),

  # Application title
  title = "MÁQUINA:", # Modelo de Análise Quantitativa para Doenças Infecciosas
  
  #Disease panels----
  get_disease_panel("Malária", "malaria", regions = regions),
  get_disease_panel("Doenças diarréicas","diarrhea", regions = regions),
  get_sobre_panel(),
)

# Server-----
server <- function(input, output, session) {

  #Malaria----
  week_input_malaria <- reactive({
    input$nweeks_malaria
  })
  
  output$malaria_map_past <- renderPlotly({
    plot_map(all_data, mozmap = mozmap, nweeks = week_input_malaria(),
             disease_name = "Malaria", type = "Observado",
             title = paste0("Últimas ", week_input_malaria(), " semanas"))
  })

  output$malaria_range_ruler <- renderPlotly({
    plot_rateguide(all_data, disease_name = "Malaria", nweeks = week_input_malaria())
  })

  output$malaria_map_future <- renderPlotly({
    plot_map(all_data, mozmap = mozmap, nweeks = week_input_malaria(),
             disease_name = "Malaria", type = "Previsto",
             title = paste0("Próximas ", week_input_malaria(), " semanas"))
  })

  output$malaria_table <- renderReactable({
    table_trend_cases(all_data, nweeks = week_input_malaria(),
                      disease_name = "Malaria")
  })

  output$malaria_table_sub <-  renderUI({
    table_trend_cases_footer(all_data, nweeks = week_input_malaria(),
                             disease_name = "Malaria")
  })
  
  lapply(1:length(regions), function(i) {
    outputId <- paste0("malaria_", i)
    output[[outputId]] <- renderPlotly({
      plot_forecast(all_data, "Malaria", regions[i])
    })
  })
  
  #Diarrhea----
  week_input_diarrhea <- reactive({
    input$nweeks_diarrhea
  })
  
  output$diarrhea_map_past <- renderPlotly({
    plot_map(all_data, mozmap = mozmap, nweeks = week_input_diarrhea(),
             disease_name = "Diarrhea", type = "Observado",
             title = paste0("Últimas ", week_input_diarrhea(), " semanas"))
  })
  
  output$diarrhea_range_ruler <- renderPlotly({
    plot_rateguide(all_data, disease_name = "Diarrhea", nweeks = week_input_diarrhea())
  })
  
  output$diarrhea_map_future <- renderPlotly({
    plot_map(all_data, mozmap = mozmap, nweeks = week_input_diarrhea(),
             disease_name = "Diarrhea", type = "Previsto",
             title = paste0("Próximas ", week_input_diarrhea(), " semanas"))
  })
  
  output$diarrhea_table <- renderReactable({
    table_trend_cases(all_data, nweeks = week_input_diarrhea(),
                      disease_name = "Diarrhea")
  })
  
  output$diarrhea_table_sub <-  renderUI({
    table_trend_cases_footer(all_data, nweeks = week_input_diarrhea(),
                             disease_name = "Diarrhea")
  })
  
  lapply(1:length(regions), function(i) {
    outputId <- paste0("diarrhea_", i)
    output[[outputId]] <- renderPlotly({
      plot_forecast(all_data, "Diarrhea", regions[i])
    })
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
