mod_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             h3(id = "world-view", class = "mt-0 pt-0", "Tests used and registered in the countries")
      )
    ),
    
    fluidRow(
      column(width = 12,
             tags$div(class = "info-container",
                      prettyRadioButtons(
                        inputId = ns("slt_category"),
                        label = NULL,
                        choices = c("Molecular Test", "Antibody RDT", "Professional Use Antigen RDT", "Self-test Antigen RDT"),
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
             tags$div(class = "info-container",
                      pickerInput(inputId = ns("slt_question"),
                        label = "Select question of interest",
                        multiple = FALSE,
                        choices = NULL,
                        selected = NULL
                      ),
                      
                      tags$span(
                        class="info-mark",
                        icon("info-circle"),
                        tags$div(
                          class = "info-mark-text",
                          tags$p('Please select which question to display when hovering on the map')
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
  rv <- reactiveValues(selected_on_map = NULL, value = NULL)
  
  # Render Map --------------------------------------------
  output$map <- renderEcharts4r( {
    req(input$slt_question)
    
    value <- input$slt_question
    theme <- "grey"
    
    df <- copy(data_map)
    #df <- df[Country != "Kosovo"]
    df <- e_country_names(df, iso2c, name)
    df[Country == "Kosovo", name := "Kosovo"]
    
    # Adjust country names
    df[, name := get_recoded_countries(name)]
    
    df$value <- df[[value]]
    if (input$slt_question %in% testing_cols) {
      colors <- get_map_colors(df[[value]])
      label <- get_map_labels(df[[value]])
      
      df[, value := ifelse(is.na(df$value), "No data", df$value)]
      df[, value := value_lookup[df$value]]
    } else {
      colors <- get_map_colors(df[[value]], yesno = FALSE)
      label <- get_map_labels(df[[value]], yesno = FALSE)
      
      df[, value := ifelse(is.na(df$value), "No data", "Data available")]
      df[, value := value_lookup2[df$value]]
    }
    
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
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          var test_cols = $('[data-id=\"mod_map-slt_question\"]').attr('title');
          var testcolArray = test_cols.split('|');
          var list = '';
          
          for (i = 0; i < testcolArray.length; i++) {
            var key = testcolArray[i];
            var paramValue = 'No data';

            if (params.data !== undefined && params.data.hasOwnProperty(key) && params.data[key] !== null) {
              paramValue = params.data[key];
            }
            list += '<li><b>' + key + '</b>: ' + paramValue + '</li>';
          }
          
          return(params.name + '</span>' + '<br>' + '<span>' + '<ul>' + list + '</ul>' + '</span>')
        }
      "))
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
      df <- df[1, ]
      
      selected_test_cols <- switch (input$slt_category,
                                    `Molecular Test` = column_choices$`Molecular Test`,
                                    `Professional Use Antigen RDT` = column_choices$`Professional Use Antigen RDT`,
                                    `Antibody RDT` = column_choices$`Antibody RDT`,
                                    `Self-test Antigen RDT` = column_choices$`Self-test Antigen RDT`
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
  
  # Event: Update question picker based on category -------
  observeEvent(input$slt_category, {
    req(input$slt_category)
    
    choices <- column_choices[[input$slt_category]]
    selected <- registration_questions[[input$slt_category]]
    
    if (input$slt_category == "Molecular Test") {
      value <- "Molecular test registered in country"
    } else if (input$slt_category == "Professional Use Antigen RDT") {
      value <- "Antigen RDTs registered in country"
    } else if (input$slt_category == "Antibody RDT") {
      value <- "Antibody RDTs registered in country"
    } else if (input$slt_category == "Self-test Antigen RDT") {
      value <- "Self tests registered for use in country"
    }
    
    # Set reactive value
    rv$value <- value
    
    # Update picker
    updatePickerInput(session = session, inputId = "slt_question", choices = choices, selected = selected)
  })
}
