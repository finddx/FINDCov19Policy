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
  title = "COVID-19 Diagnostics Policy Mapping Dashboard",
  body = tablerDashBody(
    tags$head(
      # custom CSS
      tags$link(href = "flag-icon.min.css", rel = "stylesheet"),
      tags$link(href = "imperial-urw.css", rel = "stylesheet"),
      tags$link(href = "style.css", rel = "stylesheet"),

      # tags$link(href='https://fonts.googleapis.com/css?family=Lato:400,700,300,900'),
      tags$link(href="https://fonts.googleapis.com/css2?family=Roboto:300"),


      reactR::html_dependency_react(),
      reactR::html_dependency_reacttools(),
      htmlwidgets::getDependency("reactable","reactable"),
      material_dep,
      tags$style("
        .rt-thead.-filters .rt-tr {align-items: flex-end; height: 60px;}
        .rt-thead.-filters .rt-th {overflow: visible;}
        menuitem {display: block;}

        .dropdown-menu {
          z-index: 1000001 !important;
        }

        /* Underline From Left */
        .hvr-underline-from-left {
          display: inline-block;
          vertical-align: middle;
          -webkit-transform: perspective(1px) translateZ(0);
          transform: perspective(1px) translateZ(0);
          box-shadow: 0 0 1px rgba(0, 0, 0, 0);
          position: relative;
          overflow: hidden;
        }
        .hvr-underline-from-left:before {
          content: '';
          position: absolute;
          z-index: -1;
          left: 0;
          right: 100%;
          bottom: 0;
          background: #5a2259;
          height: 4px;
          -webkit-transition-property: right;
          transition-property: right;
          -webkit-transition-duration: 0.3s;
          transition-duration: 0.3s;
          -webkit-transition-timing-function: ease-out;
          transition-timing-function: ease-out;
        }
        .hvr-underline-from-left:hover:before, .hvr-underline-from-left:focus:before, .hvr-underline-from-left:active:before {
          right: 0;
        }
        
        #rb_group .shiny-options-group {
          height: auto;
          width: 250px;
          -webkit-column-count: 2;
          -moz-column-count: 2;
          column-count: 2;
          -webkit-column-fill: balance;
          -moz-column-fill: balance;
          column-fill: balance;
          margin-top: 0px;
          margin-right: 100px;
        }
      "),

      tags$script(HTML("
        function filterRange(filter, rows) {
          return rows.filter(function(row) {
            // Don't filter on aggregated cells
            if (row._subRows) {
              return true;
            }

            if (filter.value == '') {
              return true;
            }
            return row[filter.id] == filter.value;
          })
        }

        function inputFilter(filter) {
          var _onChange = filter.onChange;
          return React.createElement(
            'div',
            null,
            React.createElement(
              MaterialUI.Select,
              {
                children: [
                            React.createElement('option', {value: ''}, ''),
                            React.createElement('option', {value: 'Yes'}, 'Yes'),
                            React.createElement('option', {value: 'No'}, 'No'),
                            React.createElement('option', {value: 'No data'}, 'No data')
                ],
                native: true,
                onChange: function onChange(event, newValue) {return _onChange(event.target.value)}
              }
            )
          )
        }
      "))
    ),
    fluidRow(
      # put everything in a huge tablerCard, to ensure nice borders
      tags$div(class="col-md-12",
               tags$div(class="card ",
                        tags$div(class="card-body",

                                 fluidRow(
                                   column(width = 12,
                                          h3(id = "about", class = "mt-0 pt-0", "About"),
                                          p(
                                            paste0("Testing policies offer critical frameworks and guidance for countries ",
                                                   "to implement their pandemic response. The unprecedented scale and pace of the COVID-19 pandemic ",
                                                   "and continuously evolving global context has influenced policy development and dissemination ",
                                                   "in a myriad of different ways. While policies may differ, and will continue to evolve, ",
                                                   "we believe there is value in centralizing these policies for the benefit of country implementers ",
                                                   "and policy makers globally. ")
                                          ),
                                          p(
                                            paste0(
                                              "This dashboard is designed to provide a global snapshot of ",
                                              "COVID testing policy based on publicly available data and documents. ",
                                              "We are working to implement regular refreshes of the data, please keep in mind that policies listed here ",
                                              "may not be up to date and official government websites and sources should always be considered the most accurate source."
                                            )
                                          )
                                   )
                                 ),

                                 fluidRow(
                                   column(width = 12,
                                          h3(id = "world-view", class = "mt-0 pt-0", "Tests registered in the country")
                                   )
                                 ),

                                 fluidRow(
                                   column(width = 12,
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
                                   column(width = 12,
                                          h3(id = "country-detail", class = "mt-0 pt-0", "Country detail"),
                                          uiOutput("ui_country_detail")
                                   )
                                 ),

                                 fluidRow(
                                   div(class = "col-sm-12",
                                       h3(id = "compare", "Diagnostic Policy Table"
                                          #tags$sup(a(a(style = "color: #aaa; font-weight: 400; margin-left: 0px;   z-index: 1000000 !important; font-size: 23px; ", "2")))
                                       ),

                                       p("This table displays the ..."),

                                       tags$div(class = "info-container",
                                                prettyRadioButtons(
                                                  inputId = "rb_group",
                                                  label = NULL,
                                                  status = "default",
                                                  inline = TRUE,
                                                  choices = c(
                                                    "by continent" = "Continent",
                                                    "by country details" = "Country",
                                                    "by income group aggregation" = "Income"
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
                                                  choices = column_choices,
                                                  selected = default_cols
                                                )
                                       ),
                                       tags$div(class = "info-container",
                                                prettyCheckbox("cb_show_data", "Show only countries with data", value = TRUE)
                                       ),
                                       div(style = "margin-top: 10px;",
                                           conditionalPanel("input.rb_group !== 'Country'",
                                                            d3Output("d3_legend", width = "100%", height = "80px"),
                                           )
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

                                          p(class = "small", paste0("The data was last updated on: ",
                                                                    format(as.Date(max(dx_policy$`Date of last update`, na.rm = TRUE)), "%e-%b-%Y")
                                                                    ))


                                   )
                                 )
                        )
               )
      )

    )
  )
)
