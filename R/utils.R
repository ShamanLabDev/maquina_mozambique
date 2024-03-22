#' Return the maximum predicted date
#' 
#' @param all_data All the model data as specified in simulate_during_devel
#'
#' @return The maximum observed date
get_maxdate = function(all_data){
  all_data %>% 
    filter(type == "Observado") %>%
    summarise(max(date)) %>% 
    pull()
}

#' Load the required data
#' 
#' @param fname Name of the file containing the data
#'
#' @return A tibble with the data
get_data = function(fname = "data.csv"){
  read_csv(fname, 
           col_types = cols(
             Region  = col_character(),
             disease = col_character(),
             .default = col_double(),
             date = col_date(format = "%Y-%m-%d"),
             type = col_character()
           )) %>% 
    filter(date > max(date) - years(1)) 
}