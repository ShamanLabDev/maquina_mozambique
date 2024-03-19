plot_map = function(db, mapdata, 
                    disease_name = "Malaria",
                    title = "Previous cases",
                    bg_color = "white", 
                    color_vals = c("forestgreen","darkslategray3","darkslategray2",
                               "gray75","gold","orange2","tomato3")){
  
  plot_db = db %>% 
    filter(disease == !!disease_name) %>% 
    left_join(mapdata, by = join_by(Region)) %>% 
    mutate(trend_fmt = scales::comma(incident_cases,accuracy = 1)) %>% 
    mutate(trend_pct = scales::percent(trend,accuracy = 1)) %>% 
    mutate(text = paste0("<b>", Region,"</b>", "<br>", trend_fmt, " (", trend_pct, ")")) %>% 
    ggplot() +
    geom_sf(aes(geometry = geometry, fill = trend, text = text), 
            color = "white", linewidth = 0.1) +
    theme_void() +
    theme(
      axis.line  = element_blank(), 
      axis.text  = element_blank(), 
      axis.ticks = element_blank(), 
      axis.title = element_blank(), 
      panel.background = element_blank(), 
      panel.border     = element_blank(), 
      panel.grid       = element_blank(), 
      panel.spacing    = unit(0, "lines"), 
      plot.background  = element_blank(), 
      legend.justification = c(0, 0), 
      legend.position  = "bottom",
    ) +
    ggtitle(title) +
    scale_fill_gradientn("",
                         labels = scales::percent_format(),
                         rescaler = ~ (. + 1)/2,
                         colors = color_vals,
                         values = c(0.0,0.25,0.499, 0.5, 0.5001,0.75, 1)) 

  #https://rpubs.com/rubenfbc/interactive_maps_b
  ggplotly(plot_db, tooltip = "text") %>% 
    config(displayModeBar = FALSE) %>% 
    style(hoverlabel = list(bgcolor = bg_color), hoveron = "text")

}
