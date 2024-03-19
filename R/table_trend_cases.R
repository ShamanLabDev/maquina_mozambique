table_trend_cases = function(data, 
                             fill_color = c("deepskyblue4","tomato3")){
reactable(
  data,
  defaultSorted = "Region",
  pagination = FALSE,
  bordered = TRUE,
  highlight = TRUE,
  columns = list(
    Region = colDef(sticky = "left", name = "Região"),
    Malaria_Cases = colDef(name = "Novos casos", 
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
    last_update = colDef(name = "Atualizado")
  ),
  defaultColDef = colDef(
    headerStyle = list(background = "black", color = "white")
  ))
}
