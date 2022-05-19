section_contact <- function() {
  fluidRow(
    column(width = 12,
           h3("Contact us"),
           p("Please kindly send any corrections, concerns or additional inputs about data displayed on the policy mapping dashboard to ",
             tags$a("ACTADiagnostics@finddx.org", href = "mailto:ACTADiagnostics@finddx.org")
           )
    )
  )
}
