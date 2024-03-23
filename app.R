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

#Source all files of additional functions
list.files("R/", pattern = "*.R", all.files = T, full.names = T) %>% 
  walk(., source)

#Get the shapefiles for the map
mozmap = preprocess_map()

#Read the data in a robust way
all_data = get_data()

# Define UI for application that draws a histogram
ui <- navbarPage(
  #Theme
  theme = bs_theme(preset = "sandstone", version = 5),

  # Application title
  title = "MÁQUINA:", # Modelo de Análise Quantitativa para Doenças Infecciosas
  
  #Each of the panel's
  get_disease_panel("Malária", "malaria"),
  get_disease_panel("Doenças diarréicas","diarrhea"),
  get_sobre_panel(),
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  week_input <- reactive({
    input$nweeks
  })
  
  output$malaria_map_past <- renderPlotly({
    plot_map(all_data, mozmap = mozmap, nweeks = week_input(), 
             disease_name = "Malaria", type = "Observado",
             title = paste0("Últimas ", week_input(), " semanas"))
  })
  
  output$malaria_range_ruler <- renderPlotly({
    plot_rateguide(all_data, disease_name = "Malaria", nweeks = week_input())
  })
  
  output$malaria_map_future <- renderPlotly({
    plot_map(all_data, mozmap = mozmap, nweeks = week_input(),
             disease_name = "Malaria", type = "Previsto",
             title = paste0("Próximas ", week_input(), " semanas"))
  })
  
  output$malaria_table <- renderReactable({
    table_trend_cases(all_data, nweeks = week_input(), 
                      disease_name = "Malaria")
  })
  
  output$malaria_table_sub <-  renderUI({
    table_trend_cases_footer(all_data, nweeks = week_input(), 
                             disease_name = "Malaria")
  })
  
  # Number of plots to generate
  num_plots <- 5
  
  # Initialize list to store plot render functions
  plot_outputs <<- list()
  
  # Generate plots and add render functions to list
  for (i in 1:num_plots) {
    region <- paste0("Region ", i)
    plot_output_name <- paste0("plot", i)
    output[[plot_output_name]] <- renderPlotly({
      generate_plot(region)
    })
    plot_outputs[[i]] <<- plot_output_name
  }
  
  # Render the dynamically generated plotlyOutput elements
  output$plot_outputs <- renderUI({
    plot_outputs <- lapply(plot_outputs, function(plot_output_name) {
      plotlyOutput(plot_output_name)
    })
    do.call(tagList, plot_outputs)
  })
  
  # output$plots_combined <- 
  #   map(set_names(unique(all_data$Region)),
  #       ~renderPlotly({
  #         plot_forecast(all_data, last_updated_date = last_updated_date,
  #                       region = ., disease_name = "Malaria")
  #           }, env = output$plots_combined))
  # 
  # output$plot_output <- renderUI({
  #    uiOutput("plots_combined")
  # })
  
  # output$diarrhea_plot <- renderPlotly({
  #   plot_forecast(all_data, last_updated_date = last_updated_date,
  #                 disease_name = "Diarrhea")
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)
