mod_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             h3(id = "world-view", class = "mt-0 pt-0", "Are tests registered in the country?")
      )
    ),
    
    fluidRow(
      column(width = 12,
             tags$div(class = "info-container",
                      prettyRadioButtons(
                        inputId = ns("slt_category"),
                        label = NULL,
                        choices = c("Molecular Test", "Antigen RDT", "Antibody RDT", "Self Test"),
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
             
             echarts4rOutput(ns("map"), height = "450px"),
             
             tags$div(class = "info-container", style = "margin-top: -50px;",
                      prettySwitch(
                        inputId = ns("i_roam"),
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
             uiOutput(ns("ui_country_detail"))
      )
    )
  )
}

mod_map_server <- function(input, output, session) {
  ns <- session$ns
  
  # Reactive values ---------------------------------------
  rv <- reactiveValues(selected_on_map = NULL)
  
  # Render Map --------------------------------------------
  output$map <- renderEcharts4r( {
    req(input$slt_category)
    
    if (input$slt_category == "Molecular Test") {
      value <- "Molecular test registered in country"
    } else if (input$slt_category == "Antigen RDT") {
      value <- "Antigen RDTs registered in country"
    } else if (input$slt_category == "Antibody RDT") {
      value <- "Antibody RDTs registered in country"
    } else if (input$slt_category == "Self Test") {
      value <- "Self tests registered for use in country"
    }
    
    theme <- "grey"
    
    df <- copy(data_map)
    #df <- df[Country != "Kosovo"]
    df <- e_country_names(df, iso2c, name)
    df[Country == "Kosovo", name := "Kosovo"]
    
    # Adjust country names
    df[, name := dplyr::recode(name,
                               "Bahamas" = "The Bahamas",
                               "Dominican Rep." = "Dominican Republic",
                               "Tanzania" = "United Republic of Tanzania",
                               "Eq. Guinea" = "Equatorial Guinea",
                               "Timor-Leste" = "East Timor",
                               "Solomon Is." = "Solomon Islands",
                               "United States" = "United States of America",
                               "Dem. Rep. Congo" = "Democratic Republic of the Congo",
                               "Congo, Dem. Rep." = "Democratic Republic of the Congo",
                               "Congo" = "Republic of the Congo",
                               "W. Sahara" = "Western Sahara",
                               "S. Sudan" = "South Sudan",
                               "Korea" = "North Korea",
                               "Dem. Rep. Korea" = "South Korea",
                               "Guinea-Bissau" = "Guinea Bissau",
                               "Serbia" = "Republic of Serbia",
                               
                               "Myanmar (Burma)" = "Myanmar",
                               "Laos" = "Lao PDR",
                               "Côte d’Ivoire" = "Ivory Coast",
                               "Czech Rep." = "Czech Republic",
                               "Eswatini" = "Swaziland",
                               "Falkland Islands" = "Falkland Is.",
                               "South Georgia & South Sandwich Islands" = "S. Geo. and S. Sandw. Is.",
                               "French Southern Territories" = "French Southern and Antarctic Lands",
                               "British Indian Ocean Territory" = "Br. Indian Ocean Ter.",
                               "Bosnia and Herz." = "Bosnia and Herzegovina",
                               "North Macedonia" = "Macedonia",
                               "Heard & McDonald Islands" = "Heard I. and McDonald Is.",
                               "Micronesia (Federated States of)" = "Micronesia",
                               "Trinidad & Tobago" = "Trinidad and Tobago",
                               "St. Vincent & Grenadines" = "St. Vin. and Gren.",
                               "St. Lucia" = "Saint Lucia",
                               "Antigua & Barbuda" = "Antigua",
                               "U.S. Virgin Islands" = "U.S. Virgin Is.",
                               "Faroe Islands" = "Faeroe Is.",
                               "Åland Islands" = "Aland",
                               "Central African Rep." = "Central African Republic"
    )]
    
    colors <- c(
      if ("No data" %in% df[[value]]) {
        "#cbcbcb"
      },
      if ("No" %in% df[[value]]) {
        "#cd4651"
      },
      if ("Yes" %in% df[[value]]) {
        "#44abb6"
      }
    )
    
    label <- list(
      if ("No data" %in% df[[value]]) {
        list(min = 1, max = 1, label = "No data")
      },
      if ("No" %in% df[[value]]) {
        list(min = 2, max = 2, label = "No")
      },
      if ("Yes" %in% df[[value]]) {
        list(min = 3, max = 3, label = "Yes")
      }
    )
    label[sapply(label, is.null)] <- NULL
    
    df$value <- df[[value]]
    df[, value := ifelse(is.na(df$value), "No data", df$value)]
    df[, value := value_lookup[df$value]]
    
    selected_test_cols <- switch (input$slt_category,
                                  `Molecular Test` = column_choices$`Molecular testing`,
                                  `Antigen RDT` = setdiff(column_choices$`Antigen testing`, "Any limitations on who can use antigen RDTs"),
                                  `Antibody RDT` = column_choices$`Antibody testing`,
                                  `Self Test` = column_choices$`Self testing`
    )
    
    df %>%
      e_charts(name, dispose = FALSE) %>% 
      e_map_register("WORLD", geojson) %>% 
      e_map(map = "WORLD", roam = input$i_roam, bind = "updated", zoom = 1.20, center = c(0, 10), data = purrr::transpose(df)) %>% 
      # e_map_("value", roam = input$i_roam, bind = "updated", zoom = 1.20, center = c(0, 10),
      #        #selectedMode = "multiple",
      #        data = purrr::transpose(df)
      # ) %>%
      e_visual_map_(
        "value",
        type = "piecewise",
        bottom = "20%",
        left = "0%",
        calculable = FALSE,
        pieces = label,
        
        inRange = list(color = colors), # scale colors
      ) %>%
      e_theme(theme) %>%
      e_tooltip(formatter = htmlwidgets::JS(sprintf("
        function(params) {
          console.log(params);
          var value;
          
          if (params.value === 0) {
            value = 'NA';
          } else if (params.value === 1) {
            value = 'No data';
          } else if (params.value === 2) {
            value = 'No';
          } else if (params.value === 3) {
            value = 'Yes';
          } else {
            value = '';
          };
          
          var test_cols = '%s';
          var testcolArray = test_cols.split('|');
          var list = '';
          
          for (i = 0; i < testcolArray.length; i++) {
            var key = testcolArray[i];
            var paramValue = 'No data';
            
            if (params.data.hasOwnProperty(key) && params.data[key] !== null) {
              paramValue = params.data[key];
            }
            list += '<li><b>' + key + '</b>: ' + paramValue + '</li>';
          } 
          
          //var links = (params.data['Policy links']);
          
          //'</h4>' + '%s' + '<span style=\"float: right;\">' + '<b>' + value + '</b>' + 
          return(params.name + '</span>' + '<br>' + '<span>' + '<ul>' + list + '</ul>' + '</span>')
        }
      ", paste0(selected_test_cols, collapse = "|"), value))#, 
                # position = JS("function (pos, params, dom, rect, size) {
                #   // tooltip will be fixed on the right if mouse hovering on the left,
                #   // and on the left if hovering on the right.
                #   var obj = {top: 60};
                #   obj[['left', 'right'][+(pos[0] < size.viewSize[0] / 2)]] = 5;
                #   return obj;
                # }"),
                # backgroundColor = 'rgba(255, 255, 255, 0.6)'
      )
  })
  
  # Event: User clicked on map ----------------------------
  observeEvent(input$map_clicked_data, {
    rv$selected_on_map <- input$map_clicked_data$iso2c
  })
  
  # Render country detail ---------------------------------
  output$ui_country_detail <- renderUI( {
    df_full <- copy(data_map)
    #df_full <- df_full[Country != "Kosovo"]
    df_full <- e_country_names(df_full, iso2c, name)
    
    if (!is.null(rv$selected_on_map)) {
      # Get ISO code based on name
      #iso_code <- countrycode::countryname(rv$selected_on_map, destination = "iso2c")
      
      # Filter based on ISO
      df <- df_full[iso2c == rv$selected_on_map, ]
      
      selected_test_cols <- switch (input$slt_category,
                                    `Molecular Test` = column_choices$`Molecular testing`,
                                    `Antigen RDT` = column_choices$`Antigen testing`,
                                    `Antibody RDT` = column_choices$`Antibody testing`
      )
      
      test_list <- tags$ul(
        tagList(
          lapply(selected_test_cols, function(x) {
            tags$li(paste0(x, ": "), tags$b(ifelse(is.na(df[[x]]), "No data", df[[x]])))
          })
        )
      )
      
      # Gather policy links
      policy_links_cols <- str_subset(names(df), pattern = "^Policy Links [0-9]+")
      links_list <- lapply(policy_links_cols, function(x) {
        df[[x]]
      })
      
      links_list[sapply(links_list, is.na)] <- NULL
      
      button_list <- div(lapply(seq_along(links_list), function(i) {
        lnk <- str_remove_all(domain(links_list[[i]]), pattern = "^www\\.") 
        tags$a(h4(lnk, class = "btn btn-default action-button hvr-underline-from-left", 
                  style = "background-color: #72a6b8; color:white;"), target = "_blank",
               href = links_list[[i]])
      }))
      
      names_df <- na.omit(df_full[order(name), .(name, iso2c)])
      
      div(style = "background-color: #4991a3; color: white; padding: 20px;", class = "container",
          div(class = "row",
              div(class = "col-sm-6",
                  p("Select a country on the map to see further details:"),
                  pickerInput(inputId = ns("slt_secondary_detail"), label = "", inline = TRUE,
                              choices = setNames(names_df$iso2c, nm = names_df$name), selected = isolate(rv$selected_on_map)),
                  br(),
                  test_list
              ),
              div(class = "col-sm-6",
                  span(
                    "Links:", tags$i(class="fas fa-external-link-alt")
                  ),
                  tags$br(),
                  button_list
              )
          )
      )
    } else {
      names_df <- na.omit(df_full[order(name), .(name, iso2c)])
      div(style = "background-color: #4991a3; color: white; padding: 20px;", class = "container",
          div(class = "row",
              div(class = "col-sm-6",
                  p("Select a country on the map to see further details:"),
                  pickerInput(inputId = ns("slt_secondary_detail"), label = "", inline = TRUE,
                              choices = setNames(names_df$iso2c, nm = names_df$name), selected = NULL)
              ),
              div(class = "col-sm-6",
                  span(
                    "Links:", tags$i(class="fas fa-external-link-alt")
                  )
              )
          )
      )
    }
  })
  
  # Event: Dropdown used to select country ----------------
  observeEvent(input$slt_secondary_detail, {
    req(input$slt_secondary_detail)
    
    rv$selected_on_map <- input$slt_secondary_detail
  })
}
