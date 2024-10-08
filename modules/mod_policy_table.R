mod_policy_table_ui <- function(id, title, column_choices, default_cols, last_update_date) {
  ns <- NS(id)
  tagList(
    fluidRow(
      div(class = "col-sm-12",
          h3(title),
          
          p("The table below displays therapeutics access data:"),
          
          tags$div(class = "info-container", style = "display: flex; align-items: flex-end;",
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
                   ),
                   tags$div(prettyCheckbox(ns("cb_show_data"), "Show only countries with data", value = TRUE), style = "margin-left: 20px;")
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
             p(class = "small", paste0("The data was last updated on: ",
                                       format(as.Date(last_update_date), "%e %b %Y")
                                       # format(as.Date(max(dx_policy$`Date of last update`, na.rm = TRUE)), "%e%b%Y")
             ))
      )
    )
  )
}

mod_policy_table_server <- function(input, output, session, data, main_cols, column_choices, add_policy_testing = TRUE) {
  ns <- session$ns
  
  # Reactive: Policy data ---------------------------------
  policy_data <- reactive( {
    req(!is.null(input$cb_show_data))
    
    df <- data
    
    # Filter out na rows if needed
    if (input$cb_show_data) {
      sdcols <- main_cols[main_cols %in% colnames(df)]
      na_rows <- df[, rowSums(sapply(.SD, function(x) is.na(x) | x %in% c("No data", "Not applicable"))), .SDcols = sdcols]
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
    df <- df[, colnames(df) %in% c(input$cb_selected_cols, "Flag", "Country"), with = FALSE]
    
    testing_cols_list <- lapply(column_choices, function(x) x[x %in% names(df)])[-1]
    
    # Add policy testing if needed
    if (isTRUE(add_policy_testing)) {
      policy_testing <- colnames(df)[colnames(df) %in% "COVID-19 testing strategy available"]
      testing_cols_list <- c(`Testing policy` = policy_testing, testing_cols_list)
    }
    
    # Gray columns
    gray_columns <- unlist(testing_cols_list[as.logical(cumsum(
      unlist(lapply(testing_cols_list, length)) > 0
    ) %% 2)])
    
    # Columns list
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
    
    # Column group list
    gray_column_groups <- cumsum(
      unlist(lapply(testing_cols_list, length)) > 0
    ) %% 2
    
    columnGroups_list <- mapply(testing_cols_list, gray_column_groups, names(testing_cols_list),
                                SIMPLIFY = FALSE, USE.NAMES = FALSE, 
                                FUN = function(elem, grp, nm) {
      if (length(elem) > 0) {
        colGroup(name = nm, columns = elem, 
                 headerStyle = if (grp) list(`background-color` = "#f7f7f7"))
      }
    })
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
    
    range_filter_cols <- c("Continent", "Income", "COVID-19 testing strategy available", unlist(column_choices[-1], use.names = FALSE))
    
    #filter_cols <- which(colnames(df) %in% testing_cols)
    columns_name <- sapply(out$x$tag$attribs$columns, `[[`, "name")
    
    for (i in range_filter_cols) {
      if (i %in% columns_name) {
        idx <- which(i == columns_name)
        out$x$tag$attribs$columns[[idx]]$filterMethod <- htmlwidgets::JS("filterRange")
        out$x$tag$attribs$columns[[idx]]$Filter <- build_filter_range(df[[i]])
      }
    }
    
    return(out)
  })
  
  # Event: Download selected rows -------------------------
  output$lnk_download_selected <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      req(policy_data())
      df <- copy(policy_data())
    
      # Country
      df <- df[, colnames(df) %in% input$cb_selected_cols, with = FALSE]
      if (length(selected() > 0L)) {
        df <- df[selected()]
      }
      
      fwrite(x = df, file = file)
    }
  )
  
  # Event: Download raw data ------------------------------
  output$lnk_download_raw <- downloadHandler(
    filename = function() {
      paste('raw_data-', Sys.Date(), '.xlsx', sep = '')
    },
    content = function(file) {
      if (isTRUE(add_policy_testing)) {
        file.copy(from = file.path(content_path, "dx_policy.xlsx"), to = file)
      } else {
        file.copy(from = file.path(content_path, "tx_policy.xlsx"), to = file)
      }
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
