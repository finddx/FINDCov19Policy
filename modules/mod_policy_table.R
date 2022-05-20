mod_policy_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      div(class = "col-sm-12",
          h3(id = "compare", "Diagnostic Policy Table"),
          
          p("The table below displays diagnostics policy data:"),
          
          tags$div(class = "info-container",
                   prettyRadioButtons(
                     inputId = ns("rb_group"),
                     label = NULL,
                     status = "default",
                     inline = FALSE,
                     choices = c(
                       "by continent aggregation" = "Continent",
                       "by income group aggregation" = "Income",
                       "by country details" = "Country"
                     )
                   ),
                   tags$span(
                     class = "info-mark",
                     icon("info-circle"),
                     style = "margin-left: -70px; margin-top: -3px;",
                     tags$div(
                       class = "info-mark-text",
                       tags$p('Please select to display the table by country, continent or income classification')
                     )
                   )
          ),
          
          tags$div(class = "info-container",
                   pickerInput(
                     inputId = ns("cb_selected_cols"),
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
                   prettyCheckbox(ns("cb_show_data"), "Show only countries with data", value = TRUE)
          ),
          div(style = "margin-top: 10px;",
              conditionalPanel("input.rb_group !== 'Country'",
                               d3Output(ns("d3_legend"), width = "100%", height = "80px"),
              )
          ),
          reactable::reactableOutput(ns("tbl_country_list"))
      )
    ),
    
    fluidRow(
      column(width = 12,
             br(),
             h3("Download the data"),
             p("The selected data can be downloaded from", downloadLink(ns("lnk_download_selected"), label = "here,"), 
               "and the raw dataset can be downloaded from", downloadLink(ns("lnk_download_raw"), label = "here.")),
             p(class = "small", paste0("The data were last updated on: ",
                                       format(as.Date(max(dx_policy$`Date of last update`, na.rm = TRUE)), "%e%b%Y")
             ))
      )
    )
  )
}

mod_policy_table_server <- function(input, output, session) {
  ns <- session$ns
  
  # Reactive: Policy data ---------------------------------
  policy_data <- reactive( {
    req(!is.null(input$cb_show_data))
    
    df <- dx_policy
    
    # Filter out na rows if needed
    if (input$cb_show_data && input$rb_group == "Country") {
      na_rows <- df[, rowSums(sapply(.SD, is.na)), .SDcols = testing_cols[testing_cols %in% colnames(df)]]
      df <- df[na_rows == 0]
    }
    
    df
  })
  
  # Reactive: Selected countries --------------------------
  selected <- reactive(getReactableState("tbl_country_list", "selected"))
  
  # Render policy table -----------------------------------
  output$tbl_country_list <- reactable::renderReactable( {
    req(policy_data())
    df <- copy(policy_data())
    
    # Select only cols to show
    df <- df[, colnames(df) %in% c(input$cb_selected_cols, "Flag", "Country", input$rb_group), with = FALSE]
    
    policy_testing <- colnames(df)[colnames(df) == "COVID-19 testing strategy available"]
    molecular_testing <- colnames(df)[colnames(df) %in% column_choices$`Molecular testing`]
    antigen_testing <- colnames(df)[colnames(df) %in% column_choices$`Antigen testing`]
    antibody_testing <- colnames(df)[colnames(df) %in% column_choices$`Antibody testing`]
    self_testing <- colnames(df)[colnames(df) %in% column_choices$`Self testing`]
    
    testing_cols_list <- list(policy_testing, molecular_testing, antigen_testing, antibody_testing, self_testing)
    gray_columns <- unlist(testing_cols_list[as.logical(cumsum(
      unlist(lapply(testing_cols_list, length)) > 0
    ) %% 2)])
    
    # Columns list
    if (input$rb_group == "Country") {
      columns_list <- list(
        Flag = colDef(show = FALSE),
        Country = colDef(html = TRUE, cell = JS("
                  function(cellInfo) {
                    var elem = cellInfo.row['Flag'] + cellInfo.value
                    return elem;
                  }
                ")),
        `Policy links` = colDef(html = TRUE, minWidth = 1000)
      )
      columns_list <- c(
        columns_list,
        sapply(gray_columns, simplify = FALSE, USE.NAMES = TRUE, function(x) {
          colDef(
            style = function(value) {
              list(`background-color` = "#f7f7f7")
            },
            headerStyle = list(`background-color` = "#f7f7f7")
          )
        })
      )
      
      columns_list <- columns_list[names(columns_list) %in% c(colnames(df), "Flag", "Country")]
    } else {
      # Aggregate: Continent/Income Group
      testing_df_cols <- testing_cols[testing_cols %in% colnames(df)]
      
      # Convert values
      df[, (testing_df_cols) := lapply(.SD, plyr::mapvalues, 
                                       from = c("No, but used", "In the process of registration", "No Data"), 
                                       to = c("No", "No", "No data")), .SDcols = testing_df_cols]
      
      # Convert NA to No data
      df[, (testing_df_cols) := lapply(.SD, function(x) ifelse(is.na(x), "No data", as.character(x))), .SDcols = testing_df_cols]
      
      df <- lapply(na.omit(unique(df[[input$rb_group]])), function(category) {
        ds <- lapply(testing_df_cols, function(col) {
          df2 <- df[get(input$rb_group) == category, ]
          df2[, list(get_table(df2[[col]]))]
        })
        
        if (length(ds) > 0) {
          cbind.data.frame(
            paste0(category, " (", "n = ", df[get(input$rb_group) == category, .N], ")"),
            ds
          )
        } else {
          data.table(paste0(category, " (", "n = ", df[get(input$rb_group) == category, .N], ")"))
        }
      }) %>%
        rbindlist(use.names = FALSE)
      
      colnames(df) <- c(input$rb_group, testing_df_cols)
      
      columns_list <- sapply(testing_df_cols, simplify = FALSE, USE.NAMES = TRUE, function(x) {
        colDef(
          cell = function(value, index) {
            sparkline(df[[x]][[index]], 
                      sliceColors = c("#cbcbcb", "#cd4651", "#44abb6"), 
                      type = "pie", height = "35px")
          },
          style = if (x %in% gray_columns) {
            function(value) {
              list(`background-color` = "#f7f7f7")
            }
          },
          headerStyle = if (x %in% gray_columns) {
            list(`background-color` = "#f7f7f7")
          }
        )
      })
    }
    
    # Column group list
    gray_column_groups <- cumsum(
      unlist(lapply(list(policy_testing, molecular_testing, antigen_testing, antibody_testing, self_testing), length)) > 0
    ) %% 2
    
    columnGroups_list <- list(
      if (length(policy_testing) > 0) {
        colGroup(name = "Testing policy", columns = policy_testing, 
                 headerStyle = if (gray_column_groups[1]) list(`background-color` = "#f7f7f7"))
      },
      if (length(molecular_testing) > 0) {
        colGroup(name = "Molecular testing", columns = molecular_testing,
                 headerStyle = if (gray_column_groups[2]) list(`background-color` = "#f7f7f7"))
      },
      if (length(antigen_testing) > 0) {
        colGroup(name = "Antigen RDTs", columns = antigen_testing,
                 headerStyle = if (gray_column_groups[3]) list(`background-color` = "#f7f7f7"))
      },
      if (length(antibody_testing) > 0) {
        colGroup(name = "Antibody RDTs", columns = antibody_testing,
                 headerStyle = if (gray_column_groups[4]) list(`background-color` = "#f7f7f7"))
      },
      if (length(self_testing) > 0) {
        colGroup(name = "Self testing", columns = self_testing,
                 headerStyle = if (gray_column_groups[5]) list(`background-color` = "#f7f7f7"))
      }
    )
    
    columnGroups_list[sapply(columnGroups_list, is.null)] <- NULL
    
    out <- reactable(df,
                     selection = "multiple",
                     onClick = "select",
                     defaultSorted = input$rb_group,
                     searchable = TRUE,
                     filterable = TRUE,
                     showSortable = TRUE,
                     pagination = TRUE,
                     paginationType = "simple",
                     showPageInfo = FALSE,
                     showPageSizeOptions = TRUE, 
                     #pageSizeOptions = c(10, 25, 50, 100, 200, 500),
                     defaultPageSize = 10,
                     
                     theme = reactableTheme(
                       rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "none"),
                       headerStyle = list(fontWeight = 700, fontSize = "small"),
                       searchInputStyle = list(
                         width = "100%"
                       )
                     ),
                     language = reactableLang(
                       searchPlaceholder = "Filter country, continent or income group",
                       noData = "No entries for this filter",
                       pagePrevious = "\u276e",
                       pageNext = "\u276f",
                     ),
                     columns = columns_list,
                     columnGroups = columnGroups_list
    )
    
    if (input$rb_group == "Country") {
      range_filter_cols <- c("Continent", "Income", "COVID-19 testing strategy available", testing_cols)
      
      #filter_cols <- which(colnames(df) %in% testing_cols)
      columns_name <- sapply(out$x$tag$attribs$columns, `[[`, "name")
      
      for (i in range_filter_cols) {
        if (i %in% columns_name) {
          idx <- which(i == columns_name)
          out$x$tag$attribs$columns[[idx]]$filterMethod <- htmlwidgets::JS("filterRange")
          out$x$tag$attribs$columns[[idx]]$Filter <- build_filter_range(df[[i]])
        }
      }
    }
    
    return(out)
  })
  
  # Event: Download selected rows -------------------------
  output$lnk_download_selected <- downloadHandler(
    filename = function() {
      if (input$rb_group == "Country") {
        file_extension <- "csv"
      } else {
        file_extension <- "xlsx"
      }
      paste('data-', Sys.Date(), '.', file_extension, sep='')
    },
    content = function(file) {
      req(policy_data())
      df <- copy(policy_data())
      
      if (input$rb_group == "Country") {
        # Country
        df <- df[, colnames(dx_policy) %in% input$cb_selected_cols, with = FALSE]
        if (length(selected() > 0L)) {
          df <- df[selected()]
        }
        
        fwrite(x = df, file = file)
      } else {
        # Continent / Income group
        categories <- as.character(na.omit(unique(df[[input$rb_group]])))
        if (length(selected() > 0L)) {
          categories <- categories[selected()]
        }
        testing_df_cols <- testing_cols[testing_cols %in% colnames(df)]
        
        # Convert values
        df[, (testing_df_cols) := lapply(.SD, plyr::mapvalues, 
                                         from = c("No, but used", "In the process of registration", "No Data"), 
                                         to = c("No", "No", "No data")), .SDcols = testing_df_cols]
        
        # Convert NA to No data
        df[, (testing_df_cols) := lapply(.SD, function(x) ifelse(is.na(x), "No data", as.character(x))), .SDcols = testing_df_cols]
        
        # Aggregation (Yes, No, No data percentages)
        df <- lapply(categories, function(category) {
          ds <- lapply(testing_df_cols, function(col) {
            data.table(
              df[get(input$rb_group) == category, paste0(round(sum(get(col) == "Yes") / .N * 100, 1), "%")],
              df[get(input$rb_group) == category, paste0(round(sum(get(col) == "No") / .N * 100, 1), "%")],
              df[get(input$rb_group) == category, paste0(round(sum(get(col) == "No data") / .N * 100, 1), "%")]
            ) %>% 
              setnames(new = paste0(col, " (", c("Yes", "No", "No data"), ")"))
          })
          
          cbind.data.frame(
            paste0(category, " (", "n = ", df[get(input$rb_group) == category, .N], ")"),
            ds
          )
        }) %>%
          rbindlist(use.names = FALSE)
        
        # Write xlsx
        wb <- createWorkbook()
        addWorksheet(wb, sheetName = input$rb_group)
        
        # First row
        writeData(wb, x = t(c(input$rb_group, rep(testing_df_cols, each = 3))), sheet = 1, 
                  colNames = FALSE, rowNames = FALSE)
        
        # Merge cells
        no_of_cols <- 1 + length(testing_df_cols) * 3
        
        for (i in seq(from = 2, to = no_of_cols, by = 3)) { 
          mergeCells(wb, sheet = 1, cols = seq(from = i, to = i + 2), rows = 1)
        }
        
        mergeCells(wb, sheet = 1, cols = 1, rows = 1:2)
        
        # Add second row
        writeData(wb, x = t(rep(c("Yes (%)", "No (%)", "No data (%)"), times = length(testing_df_cols))), sheet = 1,
                  startRow = 2, startCol = 2,
                  colNames = FALSE, rowNames = FALSE)
        
        # Add data
        writeData(wb, x = df, sheet = 1, startRow = 3, startCol = 1, colNames = FALSE, rowNames = FALSE)
        
        # Styling
        addStyle(wb, sheet = 1, style = createStyle(textDecoration = "bold", halign = "center", valign = "center", wrapText = TRUE), 
                 rows = 1, cols = 1:no_of_cols)
        addStyle(wb, sheet = 1, style = createStyle(textDecoration = "bold", halign = "center", valign = "center"), 
                 rows = 2, cols = 1:no_of_cols)
        addStyle(wb, sheet = 1, style = createStyle(textDecoration = "bold", halign = "center", valign = "center", wrapText = FALSE), 
                 rows = 1, cols = 1)
        
        setColWidths(wb, sheet = 1, ignoreMergedCells = TRUE, cols = seq_len(no_of_cols), widths = "auto")
        
        saveWorkbook(wb, file = file)
      }
    }
  )
  
  # Event: Download raw data ------------------------------
  output$lnk_download_raw <- downloadHandler(
    filename = function() {
      paste('raw_data-', Sys.Date(), '.xlsx', sep='')
    },
    content = function(file) {
      file.copy(from = policy_file_path, to = file)
    }
  )
  
  # Render: Legend ----------------------------------------
  output$d3_legend <- renderD3( {
    r2d3(
      data = list(
        keys = c('No data', 'No', 'Yes'), 
        color = c("#cbcbcb", "#cd4651", "#44abb6")
      ),
      script = file.path("www", "legend.js"))
  })
}
