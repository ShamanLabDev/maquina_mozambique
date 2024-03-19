# Load necessary libraries
rm(list = ls())
library(tidyverse)
library(purrr)
library(forecast)

set.seed(9874)
predicted_weeks = 16
# Define the names of the regions in Mozambique
regions <- c("Cabo Delgado", "Gaza", "Inhambane", "Manica", "Maputo", 
             "Nampula", "Niassa", "Sofala", "Tete", "Zambézia", "Maputo (cidade)")

# Function to simulate malaria incident_cases with ARIMA behavior
simulate_incident_cases <- function(region, n_weeks = 260) {

  # Simulate weekly malaria incident_cases for n_weeks
  arval = runif(4, -0.15, 0.15)
  maval = runif(2, -0.5, 0.5)
  incident_cases = arima.sim(model = list(ar = arval, ma = maval), n = n_weeks, 
                     mean = rnorm(1, 100, 10)) +
    10*rnorm(1)*cos(2*pi*1:n_weeks/52) + 
    10*rnorm(1)*sin(2*pi*1:n_weeks/52) +
    10*rnorm(1)*cos(pi*1:n_weeks/52) + 
    10*rnorm(1)*sin(pi*1:n_weeks/52) +
    10*rnorm(1)*cos(0.5*pi*1:n_weeks/52) + 
    10*rnorm(1)*sin(0.5*pi*1:n_weeks/52)
  
  # Create a tibble with simulated incident_cases and corresponding epiweeks
  tibble(
    EpiWeek       = seq(1, n_weeks),
    Region        = rep(region, n_weeks),
    incident_cases = abs(incident_cases)
  )
}

# Simulate malaria incident_cases for each region
simulated_data <- map_df(regions, ~simulate_incident_cases(.x))
simulated_dates = 
  tibble(EpiWeek = 1:max(simulated_data$EpiWeek)) %>% 
  mutate(date = ymd("2015/01/01") + days(7*EpiWeek))

simulated_data = simulated_data %>% 
  left_join(simulated_dates, by = join_by(EpiWeek)) %>% 
  mutate(type = "Observado") %>% 
  mutate(incident_cases_low = incident_cases) %>% 
  mutate(incident_cases_upp = incident_cases) %>% 
  mutate(change = as.numeric(difftime(max(date), date, units = "weeks"))) %>% 
  mutate(incident_cases_low = if_else(change < predicted_weeks, 0.75*incident_cases_low, incident_cases_low)) %>% 
  mutate(incident_cases_upp = if_else(change < predicted_weeks, 1.25*incident_cases_upp, incident_cases_upp)) %>% 
  mutate(type = if_else(change < predicted_weeks, "Previsto", type)) %>% 
  select(-change,-EpiWeek ) %>% 
  mutate(rate = incident_cases/10000) %>% 
  mutate(rate_low = incident_cases_low/10000) %>% 
  mutate(rate_up = incident_cases_upp/10000) %>% 
  select(date, Region, starts_with("incident_cases"), starts_with("rate"), everything()) %>% 
  mutate(disease = "Malaria")
  
simulated_data = simulated_data %>% 
  bind_rows(
    simulated_data %>% mutate(disease = "Diarrhea")
  )

write_excel_csv(simulated_data, "data.csv")
