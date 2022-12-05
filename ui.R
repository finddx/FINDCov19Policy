banner <- HTML('
  <section class="hero"><span class="gradient"></span>
      <div class="content">
          <h2 class="subtitle underline">COVID-19 Diagnostics Policy and Therapeutics Access Dashboard</h2>
      </div>
  </section>
')

tabset_panel <- shiny::tabsetPanel(id = "id",
  shiny::tabPanel(title = "Diagnostics", value = "testing_rates",
    fluidRow(
      # put everything in a huge tablerCard, to ensure nice borders
      tags$div(class = "col-md-12", style = "margin: 0 -1.75rem;",
        tags$div(class = "card ",
          tags$div(class = "card-body",
            dx_section_about(),
            mod_map_ui(id = "mod_map_diagnostic", 
                       title = "Tests used and registered in the countries",
                       categories = c("Molecular Test", "Antibody RDT", "Professional Use Antigen RDT", "Self-test Antigen RDT"),
                       category_help_text = "Please select to display whether Molecular/Antigen/Antibody testing is registered for use in countries"),
            mod_policy_table_ui(id = "mod_table_diagnostic", title = "Diagnostics Policy Table",
                                column_choices = dx_column_choices, default_cols = dx_default_cols),
            section_contact()
          )
        )
      )
    )
  ),
  shiny::tabPanel(title = "Therapeutics", value = "testing_rates",
    fluidRow(
      tags$div(class = "col-md-12", style = "margin: 0 -1.75rem;",
        tags$div(class = "card ",
          tags$div(class = "card-body",
            tx_section_about(),
            mod_map_ui(id = "mod_map_treatment",
                       title = "Therapeutics available in the countries",
                       categories = c("Molnupiravir", "Nirmatrelvir/Ritonavir"),
                       category_help_text = "Please select to display if countries are eligible to access nirmatrelvir/ritonavir or molnupiravir from ACT-A partners and/or are included in MPP’s Voluntary License territory"),
            mod_policy_table_ui(id = "mod_table_treatment", title = "Treatment Access Table",
                                column_choices = tx_column_choices, default_cols = tx_default_cols),
            section_contact()
          )
        )
      )
    )
  ),
  shiny::tabPanel(title = "About", value = "about",
                  fluidRow(
                    tags$div(class = "col-md-12",
                             tags$div(class = "card ",
                                      tags$div(class = "card-body",
                                               h3(class = "mt-0 pt-0", "Content About")
                                      )
                             )
                    )
                  )
  )
)

for (i in 1:3) {
  tabset_panel$children[[1]]$children[[i]]$children[[1]] <- htmltools::tagAppendAttributes(
    tabset_panel$children[[1]]$children[[i]]$children[[1]],
    class = if (i == 1) "menu-app active" else "menu-app"
  )
}

tablerDashPage(
  navbar = tags$div(
    banner
  ),
  footer = tablerDashFooter(tagList("COVID-19 Diagnostics Policy and Therapeutics Access Dashboard"), copyrights = "Copyright FIND"),
  title = "Country testing policies",
  body = tablerDashBody(
    # HTML / CSS / JS dependencies
    load_html_dependencies(),
    
    tabset_panel
  )
)
