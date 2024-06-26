plot_forecast = function(all_data,
                         disease_name = "Malaria",
                         region = "Sofala",
                         last_updated_date = today(), 
                         color_vals = c("gray75","#78B7C5", "#E1AF00")){
  
  #plot_y_limits = get_plot_limits(all_data, disease_name)
  
  overall_trend = is_trend_increasing(all_data, region = region,
                                      disease_name = disease_name)
  
  prediction_color = ifelse(overall_trend, color_vals[3], color_vals[2])
  
  (all_data %>% 
    filter(disease == !!disease_name) %>% 
    filter(Region == !!region) %>% 
    mutate(text = paste0(
      "<i>", format(date,"%d/%b/%y"), "</i><br>",
      "Casos: ", scales::comma(incident_cases, 1)
    )) %>% 
    mutate(text = if_else(
      type == "Previsto",
      paste0(text, "<br>Intervalo: ",
             scales::comma(incident_cases_low, 1), " a ",
             scales::comma(incident_cases_upp, 1)),
      text
    )) %>% 
    ggplot() +
    geom_ribbon(aes(x = date, ymin = incident_cases_low,
                    ymax = incident_cases_upp, fill = type),
                alpha = 0.5) +
    geom_line(aes(x = date, y = incident_cases,
                  color = type)) +
    geom_line(aes(x = date, text = text, y = incident_cases,
                    color = type)) +
    labs(title = NULL,
         x = NULL,
         y = "Casos") +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5),
      strip.background = element_rect(color = "black", fill = "black"),
      strip.text = element_text(color = "white", face = "bold")
    ) +
    scale_color_manual("", values = c(color_vals[1], prediction_color)) +
    scale_fill_manual("", values = c(color_vals[1], prediction_color)) +
    scale_y_continuous(labels = scales::comma_format()) +
                       #limits = c(plot_y_limits$min_cases[1],
                      #            plot_y_limits$max_cases[1])) +
    scale_x_date(date_labels = "%b/%y")) %>% 
    ggplotly(tooltip = "text") %>% 
    config(displayModeBar = FALSE) 
}