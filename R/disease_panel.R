#' TabPanel for a certain disease
#' 
#' @description Generates the tabpanel with name `tabname` for disease
#' given by `value`.
#' 
#' @param tabname Name of the tab. 
#' @param value Value of the disease in the output
#'
#' @details The output in the server side should have the outputs specified here.
#' 
#' @return A tabPanel for a disease
get_disease_panel = function(tabname = "Malária", value = "malaria", regions){
  tabPanel(tabname,
     #Initial text----
     card(
       fluidRow(
         column(12,
            h2(tabname, class = "lead mt-3"),
            p("This text is automatic from the data:
               A total of 1,000 new cases are expected in the next two weeks
               with Cabo Delgado (1,181), Maputo (1,539), and Gaza (1,539) 
               seeing the largest increase. The trend is currently decreasing 
               with the highest decrements happening in Nampula, Maputo, and Sofala.")
        )),
     ),
     #Plots----
     card(
       card_header("Incidência [Total de casos]"),
       card_body(
       fluidRow(
         column(5, align="center",
                plotlyOutput(paste0(value,"_map_past"), width = "100%")
         ),
         column(2, align = "center",
                plotlyOutput(paste0(value,"_range_ruler"), width = "50%")
                ),
         column(5, align="center",
                plotlyOutput(paste0(value,"_map_future"), width = "100%")
         )
       ),
     )),
     #Slider----
     card(
       card_header(
       fluidRow(
         column(4, offset = 4,
                sliderInput(paste0("nweeks_",value), "Número de semanas:",
                            round = TRUE,
                            min = 1, max = 16, value = 2, step = 1, width = "100%"),
         )
       ))
     ),
     #Table-----
     card(
       fluidRow(
         column(12, align="center",
                reactableOutput(paste0(value,"_table"), height = "550px", width = "100%"),
         ),
         column(12, align = "right",
                htmlOutput(paste0(value, "_table_sub"))
         )
       ),
     ),
     card(
      p("This is a different paragraph that also contains numbers generated from the data"),
     ),
     fluidRow(
       column(3, align="center",
          card(
            card_header(regions[1]),
            card_body(
              plotlyOutput(outputId = paste0(value,"_", 1),
                           width = "100%", height = "200px") 
            )
          )
       ),
       column(3, align="center",
              card(
                card_header(regions[2]),
                card_body(
                  plotlyOutput(outputId = paste0(value,"_", 2),
                               width = "100%", height = "200px") 
                )
              )
       ),
       column(3, align="center",
              card(
                card_header(regions[3]),
                card_body(
                  plotlyOutput(outputId = paste0(value,"_", 3),
                               width = "100%", height = "200px") 
                )
              )
       ),
       column(3, align="center",
              card(
                card_header(regions[4]),
                card_body(
                  plotlyOutput(outputId = paste0(value,"_", 4),
                               width = "100%", height = "200px") 
                )
              )
       ),
     ),
     fluidRow(
       column(3, align="center",
              card(
                card_header(regions[5]),
                card_body(
                  plotlyOutput(outputId = paste0(value,"_", 5),
                               width = "100%", height = "200px") 
                )
              )
       ),
       column(3, align="center",
              card(
                card_header(regions[6]),
                card_body(
                  plotlyOutput(outputId = paste0(value,"_", 6),
                               width = "100%", height = "200px") 
                )
              )
       ),
       column(3, align="center",
              card(
                card_header(regions[7]),
                card_body(
                  plotlyOutput(outputId = paste0(value,"_", 7),
                               width = "100%", height = "200px") 
                )
              )
       ),
       column(3, align="center",
              card(
                card_header(regions[8]),
                card_body(
                  plotlyOutput(outputId = paste0(value,"_", 8),
                               width = "100%", height = "200px") 
                )
              )
       ),
     ),
     fluidRow(
       column(3, align="center", offset = 2,
              card(
                card_header(regions[9]),
                card_body(
                  plotlyOutput(outputId = paste0(value,"_", 9),
                               width = "100%", height = "200px") 
                )
              )
       ),
       column(3, align="center",
              card(
                card_header(regions[10]),
                card_body(
                  plotlyOutput(outputId = paste0(value,"_", 10),
                               width = "100%", height = "200px") 
                )
              )
       ),
       column(3, align="center",
              card(
                card_header(regions[11]),
                card_body(
                  plotlyOutput(outputId = paste0(value,"_", 11),
                               width = "100%", height = "200px") 
                )
              )
       ),
     ),
     #Warning card----
     card(class = "bg-dark",
        card_header("Aviso:"),
        card_body(
           p("Lembre-se de que as projecções neste painel são geradas por um modelo 
              automatizado e podem conter erros. A compreensão dos  conceitos 
              epidemiológicos é essencial para uma interpretação correcta. 
              Consulte fontes adicionais para obter o contexto."),
        )
     ),
     #Download button----
     card(
       fluidRow(
         column(6, align="center", offset = 3,
                downloadButton(
                  paste0("download_", value),
                  label = "Download PDF",
                  class = NULL,
                  icon = shiny::icon("download")
                )
         )
       ), value = value)
     )
}