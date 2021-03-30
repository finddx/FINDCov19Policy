banner <- HTML('
  <section class="hero"><span class="gradient"></span>
      <div class="content">
          <h2 class="subtitle underline">Covid-19 Policy</h2>
      </div>
  </section>
')

tablerDashPage(
  navbar = tags$div(
    banner
  ),
  footer = tablerDashFooter(tagList("FIND Covid-19 Policy"), copyrights = "Copyright FIND"),
  title = "FIND Covid-19 Policy",
  body = tablerDashBody(
    tags$head(
      # custom CSS
      tags$link(href = "flag-icon.min.css", rel = "stylesheet"),
      tags$link(href = "imperial-urw.css", rel = "stylesheet"),
      tags$link(href = "style.css", rel = "stylesheet"),
      
      # tags$link(href='https://fonts.googleapis.com/css?family=Lato:400,700,300,900'),
      tags$link(href="https://fonts.googleapis.com/css2?family=Roboto:300")
    ),
    fluidRow(
      # put everything in a huge tablerCard, to ensure nice borders
      tags$div(class="col-md-12",
               tags$div(class="card ",
                        tags$div(class="card-body",
                                 
                                 fluidRow(
                                   column(width = 12,
                                          h3(id = "world-view", class = "mt-0 pt-0", "World View",
                                             tags$sup(a(a(style = "color: #aaa; font-weight: 400; margin-left: 0px;   z-index: 1000000 !important; font-size: 23px; ", "1")))),
                                          "The map displays data as ...:",
                                   )
                                 ),
                                 
                                 fluidRow(
                                   column(width = 12,
                                          br(),
                                          
                                          tags$div(class = "info-container",
                                                   prettyRadioButtons(
                                                     inputId = "slt_category",
                                                     label = NULL, 
                                                     choices = c("Molecular Test", "Antigen RDT", "Antibody RDT"), 
                                                     status = "default",
                                                     inline = TRUE
                                                   ),
                                                   tags$span(
                                                     class="info-mark",
                                                     icon("info-circle"),
                                                     tags$div(
                                                       class = "info-mark-text",
                                                       tags$p('Please select to display whether Molecular/Antigen/Antibody testing is registered for use in countries')
                                                     )
                                                   )
                                          ),
                                          
                                          echarts4rOutput("map", height = "450px"),
                                          
                                          tags$div(class = "info-container", style = "margin-top: -50px;",
                                                   prettySwitch(
                                                     inputId = "i_roam",
                                                     label = "Zoom",
                                                     status = "default",
                                                     width = "40px",
                                                     fill = TRUE
                                                   ),
                                                   tags$span(
                                                     class="info-mark",
                                                     style = "margin-left: 50px;   z-index: 1000000 !important;",
                                                     icon("info-circle"),
                                                     tags$div(
                                                       class = "info-mark-text",
                                                       tags$p('Switch on zoom to take a closer look at the areas of interest.
                    You can select/deselect bins on the above legend and highlight the countries that fall in the data range.')
                                                     )
                                                   )
                                          )
                                   )
                                 ),
                                 
                                 fluidRow(
                                   div(class = "col-sm-12",
                                       h3(id = "compare", "DX Policy Table",
                                          tags$sup(a(a(style = "color: #aaa; font-weight: 400; margin-left: 0px;   z-index: 1000000 !important; font-size: 23px; ", "2")))
                                       ),
                                       
                                       p("This table displays the ..."),
                                       br(),
                                       
                                       tags$div(class = "info-container",
                                                prettyRadioButtons(
                                                  inputId = "rb_group",
                                                  label = NULL,
                                                  status = "default",
                                                  inline = TRUE,
                                                  choices = c(
                                                    "by country" = "Country",
                                                    "by continent" = "Continent",
                                                    "by income group" = "Income"
                                                  )
                                                ),
                                                tags$span(
                                                  class="info-mark",
                                                  icon("info-circle"),
                                                  tags$div(
                                                    class = "info-mark-text",
                                                    tags$p('Please select to display the table by country, continent or income classification')
                                                  )
                                                )
                                       ),
                                       
                                       tags$div(class = "info-container",
                                                pickerInput(
                                                  inputId = "cb_selected_cols",
                                                  label = "Select columns to show", 
                                                  multiple = TRUE,
                                                  options = list(
                                                    `actions-box` = TRUE,
                                                    size = 8,
                                                    `selected-text-format` = "count > 3"
                                                  ),
                                                  choices = list(
                                                    General = c(
                                                      "Country",
                                                      "Continent",
                                                      "Income",
                                                      "Date of last update",
                                                      "Does the country have a policy that guides covid-19 testing strategy?",
                                                      "Policy Links"
                                                    ),
                                                    `Molecular testing` = c(
                                                      "Is molecular testing registered for use in country?",
                                                      "Is molecular testing used to confirm a Covid-19 diagnosis?"
                                                    ),
                                                    `Antibody testing` = c(
                                                      "Are antibody rapid tests registered for use in country?",                                                        
                                                      "Are antibody rapid tests used to confirm a Covid-19 diagnosis?",
                                                      "Are antibody rapid tests used for serosurveillance studies of Covid-19?"
                                                    ),
                                                    `Antigen testing` = c(
                                                      "Are antigen rapid tests registered for use in country?",
                                                      "Are antigen rapid tests used to confirm Covid-19 diagnosis?",
                                                      "Are antigen rapid tests used for the testing of symptomatic patients?",
                                                      "Are antigen rapid tests used for the screening of asymptomatic patients?",
                                                      "Are antigen rapid tests used for asymptomatic contacts of known positives (i.e., contact tracing)?",
                                                      "Are antigen rapid tests used for testing of health care workers / front line staff?",
                                                      "Are antigen rapid tests used for testing at borders /points of entry?",
                                                      "Are antigen rapid tests used for testing at schools / workplaces?",
                                                      "Are antigen rapid tests used for testing for non covid-19 hospitalized patients (e.g., scheduled or elective surgery)?",
                                                      "Who is allowed to use the Ag-RDTs (only health workers etc)?"
                                                    )
                                                  ), 
                                                  selected = default_cols
                                                )
                                       ),
                                       tags$div(class = "info-container",
                                                prettyCheckbox("cb_show_data", "Show only countries with data", value = TRUE)
                                       ),
                                       reactable::reactableOutput("tbl_country_list")
                                   )
                                 ),
                                 
                                 fluidRow(
                                   column(width = 12,
                                          br(),
                                          h3("Download the data"),
                                          p("The selected data can be downloaded from", downloadLink("lnk_download_selected", label = "here.")),
                                          p("All data is available through stable links as ",
                                            a(href = "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv", target="_blank", "CSV."),
                                            "A",
                                            a(href = "https://github.com/dsbbfinddx/FINDCov19TrackerData/blob/master/processed/codebook.csv", target="_blank", "Variable codebook"),
                                            "is also available.",
                                            "All data is available on our ",
                                            a(href = "https://github.com/dsbbfinddx/data/tree/master/processed", target="_blank", "GitHub Repository.")
                                          ),
                                          
                                          p(class = "small", paste0("The data was last updated on: ..."
                                                                    #format(as.Date("2021-03-23"), "%e-%b-%Y")
                                                                    ))
                                          
                                          
                                   )
                                 )
                        )
               )
      )
      
    )
  )
)
