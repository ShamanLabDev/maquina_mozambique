#' Create reactable table for cases / trend
#' 
#' Creates the table specifiying cases and trend
#' 
#' @details 
#' The table changes as the number of weeks changes to specify the date
#' range from when the data is collected. 
#' 
#' @param all_data Data frame containing all the data
#' @param nweeks Number of weeks in the analysis 
#' @param disease_name Name of the disease in `all_data`
#' @param fill_color Colours to show increase and decrease
#' 
#' @return A reactable that includes cases and rates per region
table_trend_cases = function(all_data, nweeks = 2,
                             disease_name = "Malaria",
                             fill_color = c("#78B7C5", "#E1AF00")){
  
  dbf = all_data %>% 
    filter_nweeks(0, disease_name) %>% 
    mutate(type = "Observado") |> 
    bind_rows(
      all_data %>% 
        filter_nweeks(nweeks, disease_name) %>% 
        mutate(type = "Previsto") 
    ) |> 
    group_by(Region, type) %>% 
    summarise(
      incident_cases     = sum(incident_cases),
      incident_cases_low = sum(incident_cases_low),
      incident_cases_upp = sum(incident_cases_upp),
      rate               = sum(rate),
      rate_low           = sum(rate_low),
      rate_up            = sum(rate_up),
      .groups = "drop") %>% 
    group_by(Region) %>% 
    mutate(incident_trend = (incident_cases - lag(incident_cases))/lag(incident_cases)) %>% 
    ungroup() %>% 
    pivot_wider(id_cols = Region, names_from = type,
                values_from = c(starts_with("incident"), starts_with("rate"))) %>% 
    mutate(incident_range = 
             paste0(scales::comma(floor(incident_cases_low_Previsto)), " a ", 
                    scales::comma(ceiling(incident_cases_upp_Previsto)))) %>% 
    mutate(rate_range = 
             paste0(scales::comma(rate_low_Previsto, 0.1), " a ", 
                    scales::comma(rate_up_Previsto, 0.1))) %>% 
    select(Region, incident_trend_Previsto, incident_cases_Previsto,
           incident_range, incident_cases_Observado,  rate_Previsto, 
           rate_range, rate_Observado)
  
    reactable(
      dbf,
      defaultSorted = "Region",
      pagination = FALSE,
      bordered   = TRUE,
      highlight  = TRUE,
      columns = list(
        Region         = colDef(sticky = "left", name = "Região"),
        incident_cases_Previsto = colDef(
          name = paste0("Casos previstos dentro de ", nweeks, " semanas"), 
          minWidth = 150,
          format = colFormat(separators = TRUE, digits = 0)),
        incident_cases_Observado = colDef(
          name = paste0("Observação semana mais recente"),
          minWidth = 150,
          format = colFormat(separators = TRUE, digits = 0)),
        incident_range = colDef(
          name = paste0("Casos previstos dentro de ", nweeks, " semanas (intervalo)"),
          minWidth = 125
        ),
        incident_trend_Previsto = colDef(
          name = paste0("Tendência (semana ", nweeks, " contra mais recente)"),
          minWidth = 125,
          cell = data_bars(dbf, 
                           text_color = "gray75",
                           brighten_text = FALSE,
                           fill_color = fill_color,
                           bar_height = 25,
                           number_fmt = scales::percent,
                           force_outside = c(-1,1))),
        rate_Observado = colDef(
          name = "Taxa observada semana mais recente",
          minWidth = 125,
          format = colFormat(separators = TRUE, digits = 2)),
        rate_Previsto = colDef(
          name = paste0("Taxa prevista dentro de ", nweeks, " semanas"),
          minWidth = 125,
          format = colFormat(separators = TRUE, digits = 2)),
        rate_range = colDef(
          name = paste0("Taxa prevista dentro de ", nweeks, " semanas (intervalo)"),
          minWidth = 125
        )
      ),
      defaultColDef = colDef(
        headerClass = "card-header"
        )
      )
}

#' Create footer for trend/cases table
#' 
#' Creates the footer HTML element for the table that specifies the
#' trend and cases. 
#' 
#' @details 
#' The footer changes as the number of weeks changes to specify the date
#' range from when the data is collected. 
#' 
#' @param all_data Data frame containing all the data
#' @param nweeks Number of weeks in the analysis 
#' @param disease_name Name of the disease in `all_data`
#' 
#' @return An HTML element that works inside a `renderUI` and generates
#' the observed and predicted period dates. 
table_trend_cases_footer = function(all_data, 
                                    nweeks = 2,
                                    disease_name = "Malaria"){
  
  
  
  #Get only dates and disease of interest
  summary_data_obs  = all_data %>% filter_nweeks(0, disease_name)
  summary_data_pred = all_data %>% filter_nweeks(nweeks, disease_name)
  
  #Write the dates into footer
  HTML(
    paste0(
      "<sup><b>Observado:</b> ", get_periodo(summary_data_obs), " | ",
      "<b>Previsto:</b> ", get_periodo(summary_data_pred),"</sup>"
    )
  )
}
