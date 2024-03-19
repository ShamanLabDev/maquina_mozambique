plot_forecast = function(dbs, 
                         disease_name = "Malaria",
                         last_updated_date = today(), 
                         color_vals = c("deepskyblue4","tomato3")){
  dbs %>% 
    filter(disease == !!disease_name) %>% 
    ggplot() +
    geom_ribbon(aes(x = date, ymin = incident_cases_low, 
                    ymax = incident_cases_upp, fill = type),
                alpha = 0.5) +
    geom_line(aes(x = date, y = incident_cases, 
                  color = type)) +
    labs(title = NULL,
         caption = paste0("Atualizado: ", last_updated_date),
         x = NULL,
         y = "Casos") +
    theme_minimal() +
    facet_wrap(~Region, ncol = 4) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    ) +
    scale_color_manual("", values = color_vals) +
    scale_fill_manual("", values = color_vals) +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_x_date(date_labels = "%b/%y")
}