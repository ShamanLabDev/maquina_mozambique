#' Preprocess Mozambique Map
#'
#' This function preprocesses a shapefile map by reading it using \code{sf::read_sf} and 
#' adding a new column "Region" in portuguese
#'
#' @param mapname Character string specifying the file path to the shapefile map.
#'   Defaults to "mapfiles/moz_admbnda_adm1_ine_20190607.shp".
#'
#' @return A tibble with the original shapefile map data and an additional column "Region".
#' 
#' @details The function reads the shapefile using \code{sf::read_sf} and then applies 
#'   pattern matching to identify regions based on specific strings found in the original data.
#'   It assigns specific region names based on the patterns detected in the "ADM1_PT" column.
#'   If no patterns match, the original "ADM1_PT" value is retained.
#'   
#'
#' @examples
#' preprocess_map()
#' preprocess_map("mapfiles/moz_admbnda_adm1_ine_20190607.shp")
#'
#' @import sf
#' @import dplyr
#' @import stringr
#' @export
preprocess_map = function(mapname = "mapfiles/moz_admbnda_adm1_ine_20190607.shp"){
  read_sf(mapname) %>% 
    mutate(Region = case_when(
      ADM1_PT == "Maputo City" ~ "Maputo (cidade)",
      ADM1_PT == "Zambezia" ~ "Zambézia",
      .default = ADM1_PT
    ))
}
