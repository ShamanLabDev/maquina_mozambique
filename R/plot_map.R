plot_map = function(all_data, mozmap, nweeks = 2, maxrate,
                    disease_name = "Malaria",
                    title = "Cases",
                    type = "Observado",
                    bg_color = "white", 
                    color_vals = wesanderson::wes_palette("Zissou1")){

  
  maxdate = get_maxdate(all_data) + days(1)

  summary_data = all_data %>% 
    filter(disease == !!disease_name & type == !!type) %>% 
    filter(date  >= !!maxdate - weeks(nweeks) & 
             date <=  !!maxdate + weeks(nweeks)) %>% 
    group_by(Region) %>% 
    summarise(rate = sum(rate), .groups = "drop") %>% 
    mutate(trend_fmt = scales::comma(rate, accuracy = 0.01)) %>% 
    mutate(text = paste0("<b>", Region,"</b>", "<br>", trend_fmt,"/1000 habitantes")) %>% 
    left_join(mozmap, by = join_by(Region))

  plot_db = ggplot(summary_data) +
    geom_sf(aes(geometry = geometry, fill = rate, text = text), 
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
      legend.position  = "none",
    ) +
    ggtitle(title) + 
    scale_fill_gradientn("",
                         labels = scales::comma_format(),
                         rescaler = ~ 1 / (1 + exp(-(./nweeks -  maxrate/2))),
                         colors = color_vals,
                         values = c(0,0.1,0.5,0.6,0.8,1)) 
  
  #https://rpubs.com/rubenfbc/interactive_maps_b
  ggplotly(plot_db, tooltip = "text") %>% 
    config(displayModeBar = FALSE) %>% 
    style(hoverlabel = list(bgcolor = bg_color), hoveron = "text")

}

#' Create legend for the map plot
#' 
#' @param 
#' 
plot_rateguide = function(maxrate = 10, nweeks = 2,
                          color_vals = wesanderson::wes_palette("Zissou1")){
  
  dbf = tibble(y = seq(0, nweeks*maxrate, length.out = 100), 
               x = 1 / (1 + exp(-(y/nweeks -  maxrate/2)))) 
  
  plot_df = ggplot() +
    geom_tile(aes(x = 0, y = y, fill = x), data = dbf) +
    scale_fill_gradientn("",
                         labels = scales::comma_format(),
                         rescaler = ~ .,
                         colors = color_vals,
                         values = c(0,0.1,0.5,0.6,0.8,1)) +
    theme_minimal() +
    theme(
      axis.line.x  = element_blank(), 
      axis.text.x  = element_blank(), 
      axis.ticks.x = element_blank(), 
      axis.title.x = element_blank(), 
      panel.background = element_blank(), 
      panel.border     = element_blank(), 
      panel.grid       = element_blank(), 
      panel.spacing    = unit(0, "lines"), 
      plot.background  = element_blank(), 
      legend.justification = c(0, 0), 
      legend.position = "none"
    ) +
    ylab("Taxa por 1,000 habitantes")
  
   ggplotly(plot_df, tooltip = NULL) %>% 
    config(displayModeBar = FALSE) %>% 
    layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  
}
