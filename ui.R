banner <- HTML('
  <section class="hero"><span class="gradient"></span>
      <div class="content">
          <h2 class="subtitle underline">COVID-19 Diagnostics Policy Mapping Dashboard</h2>
      </div>
  </section>
')

tablerDashPage(
  navbar = tags$div(
    banner
  ),
  footer = tablerDashFooter(tagList("COVID-19 Diagnostics Policy Mapping Dashboard"), copyrights = "Copyright FIND"),
  title = "Country testing policies",
  body = tablerDashBody(
    # HTML / CSS / JS dependencies
    load_html_dependencies(),
    
    fluidRow(
      # put everything in a huge tablerCard, to ensure nice borders
      tags$div(class="col-md-12",
               tags$div(class="card ",
                        tags$div(class="card-body",
                                 section_about(),
                                 mod_map_ui(id = "mod_map"),
                                 mod_policy_table_ui(id = "mod_table")
                        )
               )
      )

    )
  )
)
