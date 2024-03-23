#' Create legend for the map plot
#'
#' @description Function creating a map of Mozambique with incidence rates
#' by color. The map is rendered using plotly
#' 
#' @all_data  maxrate Maximum rate per 100k individuals observed to date.
#' @param mozmap A spatial data frame with the Mozambique regional geometry
#' @param nweeks Number of weeks to present. See details. 
#' @param maxrate Maximum rate per number of individuals observed 
#' (for the color palette to assign the highest color)
#' @param disease_name Name of the disease in the data (to filter the data)
#' @param title Title for the plot
#' @param type Type of the scenario can be `"Observado"` for observed (past)
#' cases or `"Previsto"` for future expected cases.
#' @param bg_color Color for the background of the map
#' @param color_vals Color palette for the legend.
#' 
#' @details
#' The map presents the incidence rates for Mozambique for the next `nweeks`
#' number of weeks if `type == "Previsto"` and for the previous `nweeks` if
#' `type == "Observado"`
#' 
#' @return A plotly object with the legend. 
plot_map = function(all_data, mozmap, nweeks = 2, 
                    maxrate = max(all_data$rate),
                    disease_name = "Malaria",
                    title = "Cases",
                    type = c("Observado", "Previsto"),
                    bg_color = "white", 
                    color_vals = wesanderson::wes_palette("Zissou1")){

  
  #Adds 1 day so that it works also with previstos
  maxdate = get_maxdate(all_data) + days(1)

  #Obtain the data for the disease, type and dates of analysis
  summary_data = all_data %>% 
    filter(disease == !!disease_name & type == !!type[1]) %>% 
    filter(date  >= !!maxdate - weeks(nweeks) & 
             date <=  !!maxdate + weeks(nweeks)) 
  
  (
    summary_data  %>% 
      group_by(Region) %>% 
      summarise(rate = sum(rate), .groups = "drop") %>% 
      mutate(rate_fmt = scales::comma(rate, accuracy = 0.01)) %>% 
      mutate(text = paste0("<b>", Region,"</b>", "<br>", 
                           rate_fmt,"/1000 habitantes")) %>% 
      left_join(mozmap, by = join_by(Region)) %>% 
      ggplot() +
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
        labs(
          title = title,
          subtitle = get_periodo(summary_data)
        ) + 
        scale_fill_gradientn("",
                             labels = scales::comma_format(),
                             rescaler = ~ 1 / (1 + exp(-(./nweeks -  maxrate/2))),
                             colors = color_vals,
                             values = c(0,0.1,0.5,0.6,0.8,1))
  ) %>% 
    ggplotly(tooltip = "text") %>% 
      config(displayModeBar = FALSE) %>% 
      style(hoverlabel = list(bgcolor = bg_color), hoveron = "text") %>% 
      layout(title = list(
        text = paste0(title,"<br><sup>", get_periodo(summary_data),"</sup>")))

}

#' Create legend for the map plot
#'
#' @description Auxiliary function to create the legend as a separate plot
#' for the map.
#' 
#' @param  maxrate Maximum rate per 100k individuals observed to date.
#' @param  nweeks Number of weeks in the visualization.
#' @param  color_vals Color palette for the legend.
#' 
#' @return A plotly object with the legend. 
plot_rateguide = function(maxrate = 10, nweeks = 2,
                          color_vals = wesanderson::wes_palette("Zissou1")){
  
    (
      tibble(
        y = seq(0, nweeks*maxrate, length.out = 100), 
        x = 1 / (1 + exp(-(y/nweeks -  maxrate/2)))
      ) %>% 
      ggplot() +
        geom_tile(aes(x = 0, y = y, fill = x)) +
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
    ) %>% 
    ggplotly(tooltip = NULL) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
}
