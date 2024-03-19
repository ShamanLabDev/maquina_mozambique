plot_map = function(mapdata, bg_color = "white", 
                    colors = c("deepskyblue2","deepskyblue3","deepskyblue4",
                               "gray75","tomato4","tomato3","tomato2")){
  ggplot(mapdata) +
    geom_sf(aes(geometry = geometry, fill = trend), color = "white") +
    theme_void() +
    theme(
      legend.position = "none",
    ) +
    scale_fill_gradientn("",
                         rescaler = ~ (. + 1)/2,
                         colors = colors,
                         values = c(0.0,0.25,0.499, 0.5, 0.5001,0.75, 1)) 
}