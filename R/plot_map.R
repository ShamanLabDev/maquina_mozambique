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
plot_map = function(all_data, mozmap, nweeks = 0, 
                    disease_name = "Malaria",
                    title = "Cases",
                    type = c("Observado", "Previsto"),
                    bg_color = "white", 
                    color_vals = wesanderson::wes_palette("Zissou1")){

  #Get the maximum observed rate for color palette
  maxrate = get_maxrate(all_data, disease_name)
  minrate = get_minrate(all_data, disease_name)
  
  #Obtain the data for the disease, type and dates of analysis
  summary_data = filter_nweeks(all_data, nweeks, disease_name)
  
  mapdata <- summary_data  %>% 
      group_by(Region) %>% 
      summarise(rate = sum(rate), .groups = "drop") %>% 
      mutate(rate_fmt = scales::comma(rate, accuracy = 1.0)) %>% 
      mutate(text = paste0("<b>", Region,"</b>", "<br>", 
                           rate_fmt,"/100,000 habitantes"))%>% 
      left_join(mozmap, by = join_by(Region)) 
  
  #Generate maputo and save it
  temputo <- tempfile(fileext = ".png")
  maputo  <- ggplot(mapdata |> filter(Region == "Maputo (cidade)")) +
    geom_sf(aes(geometry = geometry, fill = rate, text = text),
            linewidth = 0.0) +
    theme_void() +
    theme(
      axis.line  = element_blank(), 
      axis.text  = element_blank(), 
      axis.ticks = element_blank(), 
      axis.title = element_blank(), 
      panel.background = element_blank(), 
      panel.border     = element_rect(color = "black", linewidth = 3), 
      panel.grid       = element_blank(), 
      panel.spacing    = unit(0, "lines"), 
      plot.background  = element_blank(), 
      legend.justification = c(0, 0), 
      legend.position  = "none",
    ) +
    scale_fill_gradientn("",
                         labels = scales::comma_format(),
                         rescaler = ~ (. - minrate)/(maxrate - minrate),
                         colors = color_vals,
                         values = c(0,0.1,0.5,0.6,0.8,1)) 
  ggsave(temputo, maputo, dpi = 150, width = 5, height = 4)
  
  #Create the file
  img <- readPNG(temputo)
  
  #Remove the temporary file
  file.remove(temputo)
  (ggplot(mapdata) +
        geom_sf(aes(geometry = geometry, fill = rate, text = text),
                color = bg_color, linewidth = 0.1) +
        geom_point(aes(x = 38, y = -25.5, text = text),
                   color = bg_color, fill = bg_color,
              size = 14, 
              data = mapdata |> filter(Region == "Maputo (cidade)")) +
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
                             rescaler = ~ (. - minrate)/(maxrate - minrate),
                             colors = color_vals,
                             values = c(0,0.1,0.5,0.6,0.8,1)) +
      annotate("segment", x = 32.5, xend = 36, y = -26, yend = -24.6, color = "black",
               linewidth = 0.2) +
      annotate("segment", x = 32.5, xend = 36, y = -26, yend = -26.4, color = "black",
               linewidth = 0.2) +
      annotation_raster(img, xmin = 36, xmax = 40, ymin = -27, ymax = -24)
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
plot_rateguide = function(all_data, disease_name = "Malaria", nweeks = 2,
                          color_vals = wesanderson::wes_palette("Zissou1")){
  
  #Get the maximum observed rate for color palette
  maxrate = get_maxrate(all_data, disease_name)
  minrate = get_minrate(all_data, disease_name)
  
  (
    tibble(
      y = seq(minrate, maxrate, length.out = 100), 
      x = (y - minrate)/(maxrate - minrate)
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
      ylab("Taxa por 100,000 habitantes") +
      scale_y_continuous(labels = scales::comma_format(), n.breaks = 10,
                         position = "right")
  ) %>% 
  ggplotly(tooltip = NULL) %>% 
    config(displayModeBar = FALSE) %>% 
    layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
}
