#' Return the maximum predicted date
#' 
#' @param all_data All the model data as specified in simulate_during_devel
#' @param disease_name Name of the disease of interest
#'
#' @return The maximum observed date
get_maxdate = function(all_data, disease_name = "Malaria"){
  all_data %>% 
    filter(type == "Observado" & disease == !!disease_name) %>%
    summarise(max(date)) %>% 
    pull()
}

#' Return the maximum rate for one of the diseases
#' 
#' @param all_data All the model data as specified in simulate_during_devel
#' @param disease_name Name of the disease of interest
#'
#' @return The maximum observed rate
get_maxrate = function(all_data, disease_name = "Malaria", maxdate = get_maxdate(all_data, disease_name)){
  all_data %>% 
    filter(disease == !!disease_name) %>%
    filter(type == "Previsto" | date == !!maxdate) |> 
    summarise(max(rate)) %>% 
    pull()
}

#' Return the minimum rate for one of the diseases
#' 
#' @param all_data All the model data as specified in simulate_during_devel
#' @param disease_name Name of the disease of interest
#'
#' @return The maximum observed rate
get_minrate = function(all_data, disease_name = "Malaria", maxdate = get_maxdate(all_data, disease_name)){
  all_data %>% 
    filter(disease == !!disease_name) %>%
    filter(type == "Previsto" | date == !!maxdate) |> 
    summarise(min(rate)) %>% 
    pull()
}

#' Get text explaining the analysis period
#' 
#' Returns the min and max date of the analysis period of `summary_data`
#' 
#' @param summary_data A dataset with a `date` column
#'
#' @return The date range of the dataset as text. 
get_periodo = function(summary_data){
  paste0(
    format(min(summary_data$date), "%d/%b/%Y"),
    " a ",
    format(max(summary_data$date) + days(6), "%d/%b/%Y")
  )
}

#' Get text explaining the observed period
#' 
#' Returns the min and max date of the observed period of `summary_data`
#' 
#' @param summary_data A dataset with a `date` column and 
#' a `type` column that includes whether it is `"Observado"` (observed)
#' or not.
#'
#' @return The date range of the dataset's observations as text. 
get_periodo_observado = function(summary_data){
  summary_data %>% 
    filter(type == "Observado") %>% 
    get_periodo()
}

#' Get text explaining the predicted period
#' 
#' Returns the min and max date of the predicted period of `summary_data`
#' 
#' @param summary_data A dataset with a `date` column and 
#' a `type` column that includes whether it is `"Previsto"` (predicted)
#' or not.
#'
#' @return The date range of the dataset's predictions as text. 
get_periodo_previsto = function(summary_data){
  summary_data %>% 
    filter(type == "Previsto") %>% 
    get_periodo()
}

#' Load the required data
#' 
#' @param fname Name of the file containing the data
#'
#' @return A tibble with the data for the last year
get_data = function(fname = "data.csv"){
  read_csv(fname, 
           col_types = cols(
             Region  = col_character(),
             disease = col_character(),
             .default = col_double(),
             date = col_date(format = "%m/%d/%y"),
             type = col_character()
           )) %>% 
    filter(date > max(date) - years(1)) 
}

#' Filter data to dates and disease for some weeks
#' 
#' Filters the dataset for creating the table and map. Keeps only
#' observations for a disease in a date range of `nweeks` including the
#' last observed `nweeks` and the first predicted `nweeks`. 
#' 
#' @param all_data Dataset with entry for `date`, `disease`, and `type` (if
#' type is specified)
#' @param nweeks Number of weeks of observed and/or predicted to include. 
#' @param disease_name Name of the disease to keep in the filter
#' @param type Type of the observation whether `"Previsto"` (predicted) or
#' `"Observado"`. 
#' 
#' @details If dataset only includes `"Previsto"`s or `"Observado"`s 
#' cases and no type is specified it will fail.  
#' 
#' @return A dataframe filtered for the disease in `disease_name`, for 
#' dates that range for the last `nweeks` of observed data and the first
#' `nweeks` of predicted data.  
#' 
filter_nweeks = function(all_data, nweeks, disease_name){
  
  #Adds 1 day so that it works also with previstos
  maxdate = get_maxdate(all_data, disease_name = disease_name)
  
  #Filters the data 
  summary_data = all_data %>% 
    #filter(date  >= !!maxdate - weeks(nweeks) & 
    #         date <=  !!maxdate + weeks(nweeks)) %>%
    filter(date  == !!maxdate + weeks(nweeks)) %>%   
    filter(disease == !!disease_name)
  
  return(summary_data)
}

#' Compute the trend for incident cases
#' 
#' Obtains the trend of the dataset to know whether the incident cases
#' are increasing or decreasing
#' 
#' @param all_data Dataset with entry for `date`, `disease`
#' @param nweeks Number of weeks of observed and/or predicted to include. 
#' @param disease_name Name of the disease to keep in the filter
#' 
#' @return A dataframe filtered for the disease in `disease_name`, for 
#' dates that range for the last `nweeks` of observed data and the first
#' `nweeks` of predicted data. It also contains the trend.  
#' 
is_trend_increasing = function(all_data, disease_name = "Malaria", region = "Sofala"){
  
  #Adds 1 day so that it works also with previstos
  cases = all_data %>% 
    filter(Region == !!region & disease == !!disease_name) %>% 
    group_by(type) %>% 
    filter(date == max(date)) %>% 
    ungroup() %>% 
    arrange(date) %>% 
    pull(incident_cases) 
  
  cases[2] > cases[1]

}

#' Get plot limits
#'
#' Returns the limits for the regional plot of `disease_name`
#' @param all_data A dataset with diseases given by `disease` and a collection
#' of incident cases with confidence interval given by `incident_cases_low`
#' and `incident_cases_upp`. 
#' @param disease_name Name of the disease of interest in column `disease` of
#' `all_data` 
get_plot_limits = function(all_data, disease_name){
  all_data %>% 
    filter(disease == !!disease_name) %>% 
    summarise(min_cases = min(incident_cases_low),
              max_cases = max(incident_cases_upp))
}