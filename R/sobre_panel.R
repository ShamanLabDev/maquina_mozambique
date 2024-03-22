get_sobre_panel = function(){
  tabPanel("Sobre",
           card(
             fluidRow(
               column(12,
                 h2("Brief explanation of the model here", class = "lead mt-3"),
                 p("bla bla bla"),
                 value = "about")
              )
            )
           )
}