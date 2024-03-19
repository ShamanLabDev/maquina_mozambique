# Load necessary libraries
rm(list = ls())
library(tidyverse)
library(purrr)
library(forecast)

set.seed(9874)
predicted_weeks = 10
# Define the names of the regions in Mozambique
regions <- c("Cabo Delgado", "Gaza", "Inhambane", "Manica", "Maputo", 
             "Nampula", "Niassa", "Sofala", "Tete", "Zambézia", "Maputo (cidade)")

# Function to simulate malaria cases with ARIMA behavior
simulate_malaria_cases <- function(region, n_weeks = 260) {

  # Simulate weekly malaria cases for n_weeks
  arval = runif(4, -0.15, 0.15)
  maval = runif(2, -0.5, 0.5)
  cases = arima.sim(model = list(ar = arval, ma = maval), n = n_weeks, 
                     mean = rnorm(1, 100, 10)) +
    10*rnorm(1)*cos(2*pi*1:n_weeks/52) + 
    10*rnorm(1)*sin(2*pi*1:n_weeks/52) +
    10*rnorm(1)*cos(pi*1:n_weeks/52) + 
    10*rnorm(1)*sin(pi*1:n_weeks/52) +
    10*rnorm(1)*cos(0.5*pi*1:n_weeks/52) + 
    10*rnorm(1)*sin(0.5*pi*1:n_weeks/52)
  
  # Create a tibble with simulated cases and corresponding epiweeks
  tibble(
    EpiWeek       = seq(1, n_weeks),
    Region        = rep(region, n_weeks),
    Malaria_Cases = abs(cases)
  )
}

# Simulate malaria cases for each region
simulated_data <- map_df(regions, ~simulate_malaria_cases(.x))
simulated_dates = 
  tibble(EpiWeek = 1:max(simulated_data$EpiWeek)) %>% 
  mutate(date = ymd("2015/01/01") + days(7*EpiWeek))
simulated_data = simulated_data %>% 
  left_join(simulated_dates, by = join_by(EpiWeek)) %>% 
  mutate(type = "Observado") %>% 
  mutate(low = Malaria_Cases) %>% 
  mutate(upp = Malaria_Cases) %>% 
  mutate(change = as.numeric(difftime(max(date), date, units = "weeks"))) %>% 
  mutate(low = if_else(change < predicted_weeks, 0.75*low, low)) %>% 
  mutate(upp = if_else(change < predicted_weeks, 1.25*upp, upp)) %>% 
  mutate(type = if_else(change < predicted_weeks, "Previsto", type))

write_excel_csv(simulated_data, "data.csv")
