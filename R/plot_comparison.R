plot_comparison = function(db_malaria, db_diarrhea, last_updated_date,
                           colors = c("forestgreen","black")){
  (db_malaria %>% 
     filter(type == "Observado") %>% 
     rowwise() %>% 
     mutate(prevpred = rnorm(1, Malaria_Cases, sd = 10)) %>% 
     ggplot() +
     geom_line(aes(x = date, y = prevpred, color = "Prevista")) +
     geom_point(aes(x = date, y = Malaria_Cases, color = "Observado")) +
     theme_minimal() +
     labs(title = "Malaria",
          caption = paste0("Atualizado: ", last_updated_date),
          x = NULL,
          y = "Casos") +
     facet_wrap(~Region) +
     theme(legend.position = "bottom") +
     scale_color_manual("", values = colors)) +
  (db_diarrhea %>% 
     filter(type == "Observado") %>% 
     rowwise() %>% 
     mutate(prevpred = rnorm(1, Malaria_Cases, sd = 10)) %>% 
     ggplot() +
     geom_line(aes(x = date, y = prevpred, color = "Prevista")) +
     geom_point(aes(x = date, y = Malaria_Cases, color = "Observado")) +
     theme_minimal() +
     labs(title = "Diarrhea",
          caption = paste0("Atualizado: ", last_updated_date),
          x = NULL,
          y = "Casos") +
     facet_wrap(~Region) +
     theme(legend.position = "bottom") +
     scale_color_manual("", values = c("forestgreen","black")))
}