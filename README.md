
# MÁQUINA:
## Modelo de Análise Quantitativa para Doenças Infecciosas

<!-- badges: start -->
<!-- badges: end -->

This is the code for the shiny app displaying modeling forecasts for Mozambique 
currently available at:

[https://rodrigozepeda.shinyapps.io/mozambique_test/](https://rodrigozepeda.shinyapps.io/mozambique_test/)

## Developing

The `app.R` file contains the shiny applet. 

1. From the interface (`ui`) side, each tab should be a different function.
2. From the server side, each `plot_*.R` / `table_*.R` / `paragraph_*.R` 
should be a different function.
3. Before calling both the server and ui we preprocess the data a bit. This is
done with the `preprocess`  functions

The file `simulate_during_devel.R` creates a `data.csv` file that exemplifies the
expected file for the website. 
