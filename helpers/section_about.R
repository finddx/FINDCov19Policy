dx_section_about <- function() {
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

tx_section_about <- function() {
  fluidRow(
    column(width = 12,
           h3(id = "about", class = "mt-0 pt-0", ""),
           p(
             paste0("Test-and-treat strategies that link diagnostics with therapeutics are essential to prevent hospitalization ",
                    "and death from COVID-19 for those at highest risk. Oral antivirals nirmatrelvir/ritonavir ",
                    "(strong WHO recommendation) and molnupiravir (conditional WHO recommendation) may be available to countries ",
                    "via originators and/or generic manufacturers through various mechanisms. This dashboard represents:")
           ),
           tags$ol(
             tags$li(paste0("The countries that have access to generic versions via the voluntary license territories of the ",
                            "Medicines Patent Pool's (MPP's) license agreements")),
             tags$li("The countries that have access to originator products via ACT-A partner (Global Fund and/or Unicef) procurement platforms")
           ),
           p(
             paste0("Please note that countries that are not included in the voluntary license territory may still be able to ",
                    "access generics through other pathways, depending on the Intellectual Property status of the medicine in ",
                    "the country. For further information, please refer to the ",
                    "Unitaid/WHO Country Briefing on Access Treatments for COVID-19.")
           ),
           p("Please refer to the ", 
             tags$a("WHO Therapeutics and COVID-19: living guideline", href = "https://www.who.int/publications/i/item/WHO-2019-nCoV-therapeutics-2022.4"),
             " for the latest clinical recommendation and the MPP website for the latest information on the ",
             tags$a("nirmatrelvir", href = "https://medicinespatentpool.org/licence-post/pf-07321332"),
             " and ", 
             tags$a("molnupiravir", href = "https://medicinespatentpool.org/licence-post/molnupiravir-mol"),
             " license agreements."
           )
    )
  )
}
