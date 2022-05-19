section_about <- function() {
  fluidRow(
    column(width = 12,
           h3(id = "about", class = "mt-0 pt-0", ""),
           p(
             paste0("Testing policies offer critical frameworks and guidance for countries ",
                    "to implement their pandemic response. The unprecedented scale and pace of the COVID-19 pandemic ",
                    "and continuously evolving global context has influenced policy development and dissemination ",
                    "in many different ways. While policies may differ, and will continue to evolve, ",
                    "we believe there is value in centralizing these policies for the benefit of country implementers ",
                    "and policy makers globally. ")
           ),
           p(
             paste0(
               "This dashboard is designed to provide a global snapshot of ",
               "COVID-19 testing policy based on publicly available data and documents. ",
               "We are working to implement regular refreshes of the data, please keep in mind that policies listed here ",
               "may not be up-to-date and official government websites and ressources should always be considered the most accurate source."
             )
           )
    )
  )
}
