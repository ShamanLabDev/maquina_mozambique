table_trend_cases = function(data, 
                             disease_name = "Malaria",
                             fill_color = c("deepskyblue4","tomato3")){
  
  data %>% 
    dplyr::filter(disease == !!disease_name) %>% 
    dplyr::select(-disease) %>% 
    reactable(
      defaultSorted = "Region",
      pagination = FALSE,
      bordered   = TRUE,
      highlight  = TRUE,
      columns = list(
        Region         = colDef(sticky = "left", name = "Região"),
        incident_cases = colDef(name = "Novos casos", 
                                width = 75,
                                format = colFormat(separators = TRUE, 
                                                  digits = 0)),
        trend = colDef(
          name = "Tendência",
          cell = data_bars(data, 
                           text_color = "black",
                           brighten_text = FALSE,
                           fill_color = fill_color,
                           bar_height = 25,
                           number_fmt = scales::percent,
                           force_outside = c(-1,1))),
        period = colDef(name = "Período")
      ),
      defaultColDef = colDef(
        headerStyle = list(background = "black", color = "white")
    ))
}
