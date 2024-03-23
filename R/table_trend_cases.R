table_trend_cases = function(all_data, nweeks = 2,
                             disease_name = "Malaria",
                             fill_color = c("#78B7C5", "#E1AF00")){
  
  
  #Adds 1 day so that it works also with previstos
  maxdate = get_maxdate(all_data) + days(1)
  
  #Get only dates and disease of interest
  summary_data = all_data %>% 
    filter(date  >= !!maxdate - weeks(nweeks) & 
             date <=  !!maxdate + weeks(nweeks)) %>%   
    filter(disease == !!disease_name)
  
  dbf = summary_data %>% 
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
          name = paste0("Casos previstos (", nweeks, " semanas)"), 
          minWidth = 150,
          format = colFormat(separators = TRUE, digits = 0)),
        incident_cases_Observado = colDef(
          name = paste0("Casos anteriores (", nweeks, " semanas)"),
          minWidth = 150,
          format = colFormat(separators = TRUE, digits = 0)),
        incident_range = colDef(
          name = "Casos previstos (intervalo)",
          minWidth = 125
        ),
        incident_trend_Previsto = colDef(
          name = "Tendência",
          minWidth = 125,
          cell = data_bars(dbf, 
                           text_color = "gray75",
                           brighten_text = FALSE,
                           fill_color = fill_color,
                           bar_height = 25,
                           number_fmt = scales::percent,
                           force_outside = c(-1,1))),
        rate_Observado = colDef(
          name = "Taxa observada",
          minWidth = 125,
          format = colFormat(separators = TRUE, digits = 2)),
        rate_Previsto = colDef(
          name = "Taxa prevista",
          minWidth = 125,
          format = colFormat(separators = TRUE, digits = 2)),
        rate_range = colDef(
          name = "Taxa prevista (intervalo)",
          minWidth = 125
        )
      ),
      defaultColDef = colDef(
        headerClass = "card-header"
        )
      ) 
    
}
