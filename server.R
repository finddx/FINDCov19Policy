function(input, output, session) {
  rv <- reactiveValues(selected_on_map = NULL)
  
  selected <- reactive(getReactableState("tbl_country_list", "selected"))
  
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
  
  
  observeEvent(input$map_clicked_data, {
    rv$selected_on_map <- input$map_clicked_data$name
    #print(input$map_clicked_data$name)
    # df <- dx_policy
    # if (input$cb_show_data) {
    #   na_rows <- df[, rowSums(sapply(.SD, is.na)), .SDcols = testing_cols[testing_cols %in% colnames(df)]]
    #   df <- df[na_rows == 0]
    # }
    # 
    # country <- sort(as.character(df$Country))
    # new_selected_idx <- which(country == input$map_clicked_data$name)
    # current_selection <- getReactableState("tbl_country_list", "selected")
    # 
    # updateReactable("tbl_country_list", selected = unique(c(current_selection, new_selected_idx)))
  })
  
  # Render table --------------------------------
  output$tbl_country_list <- reactable::renderReactable( {
    req(policy_data())
    df <- copy(policy_data())
    
    # Select only cols to show
    df <- df[, colnames(df) %in% c(input$cb_selected_cols, "Flag", "Country", input$rb_group), with = FALSE]
    
    policy_testing <- colnames(df)[colnames(df) == "Covid-19 testing strategy available"]
    molecular_testing <- colnames(df)[colnames(df) %in% column_choices$`Molecular testing`]
    antigen_testing <- colnames(df)[colnames(df) %in% column_choices$`Antigen testing`]
    antibody_testing <- colnames(df)[colnames(df) %in% column_choices$`Antibody testing`]
    
    testing_cols_list <- list(policy_testing, molecular_testing, antigen_testing, antibody_testing)
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
        `Policy Links` = colDef(html = TRUE, minWidth = 1000)
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
      unlist(lapply(list(policy_testing, molecular_testing, antigen_testing, antibody_testing), length)) > 0
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
        colGroup(name = "Antigen rapid tests", columns = antigen_testing,
                 headerStyle = if (gray_column_groups[3]) list(`background-color` = "#f7f7f7"))
      },
      if (length(antibody_testing) > 0) {
        colGroup(name = "Antibody rapid tests", columns = antibody_testing,
                 headerStyle = if (gray_column_groups[4]) list(`background-color` = "#f7f7f7"))
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
      range_filter_cols <- c("Continent", "Income", "Covid-19 testing strategy available", testing_cols)
      
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

  # Render Map --------------------------------------------
  output$map <- renderEcharts4r( {
    req(input$slt_category)
    
    if (input$slt_category == "Molecular Test") {
      value <- "Molecular test registered in country"
    } else if (input$slt_category == "Antigen RDT") {
      value <- "Antigen rapid tests registered in country"
    } else if (input$slt_category == "Antibody RDT") {
      value <- "Antibody rapid tests registered in country"
    }
    
    theme <- "grey"
    
    df <- copy(data_map)
    df <- df[Country != "Kosovo"]
    df <- e_country_names(df, iso2c, name)

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
      `Antigen RDT` = setdiff(column_choices$`Antigen testing`, "Any limitations on who can use antigen rapid tests"),
      `Antibody RDT` = column_choices$`Antibody testing`
    )
    
    df %>%
      e_charts(name, dispose = FALSE) %>% 
      e_map_("value", roam = input$i_roam, bind = "updated", zoom = 1.20, center = c(0, 10),
             #selectedMode = "multiple",
             data = purrr::transpose(df)
      ) %>%
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
          
          //var links = (params.data['Policy Links']);
          
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
  
  # Download selected rows ----------------------
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
        df <- df[selected(), colnames(dx_policy) %in% input$cb_selected_cols, with = FALSE]
        
        fwrite(x = df, file = file)
      } else {
        # Continent / Income group
        categories <- as.character(na.omit(unique(df[[input$rb_group]])))[selected()]
        testing_df_cols <- testing_cols[testing_cols %in% colnames(df)]
        
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
  
  # Legend --------------------------------------
  output$d3_legend <- renderD3( {
    r2d3(
      data = list(
        keys = c('No data', 'No', 'Yes'), 
        color = c("#cbcbcb", "#cd4651", "#44abb6")
      ),
      script = file.path("www", "legend.js"))
  })
  
  # Country detail ------------------------------
  output$ui_country_detail <- renderUI( {
    df_full <- copy(data_map)
    df_full <- df_full[Country != "Kosovo"]
    df_full <- e_country_names(df_full, iso2c, name)
    
    if (!is.null(rv$selected_on_map)) {
      df <- df_full[name == rv$selected_on_map, ]
      
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
        tags$a(h4(domain(links_list[[i]]), class = "btn btn-default action-button hvr-underline-from-left", 
                  style = "background-color: #72a6b8; color:white;"), target = "_blank",
               href = links_list[[i]])
      }))
      
      div(style = "background-color: #4991a3; color: white; padding: 20px;", class = "container",
          div(class = "row",
              div(class = "col-sm-6",
                  p("Select a country on the map to see further details:"),
                  pickerInput(inputId = "slt_secondary_detail", label = "", inline = TRUE,
                              choices = sort(df_full$name), selected = isolate(rv$selected_on_map)),
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
      div(style = "background-color: #4991a3; color: white; padding: 20px;", class = "container",
          div(class = "row",
              div(class = "col-sm-6",
                  p("Select a country on the map to see further details:"),
                  pickerInput(inputId = "slt_secondary_detail", label = "", inline = TRUE,
                              choices = sort(df_full$name), selected = NULL)
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
  
  observeEvent(input$slt_secondary_detail, {
    req(input$slt_secondary_detail)
    
    rv$selected_on_map <- input$slt_secondary_detail
  })
}
