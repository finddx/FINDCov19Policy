function(input, output, session) {
  selected <- reactive(getReactableState("tbl_country_list", "selected"))
  
  policy_data <- reactive( {
    req(!is.null(input$cb_show_data))
    
    df <- dx_policy
    
    # Select only rows to show
    if (input$cb_show_data) {
      na_rows <- df[, rowSums(sapply(.SD, is.na)), .SDcols = testing_cols[testing_cols %in% colnames(df)]]
      df <- df[na_rows == 0]
    }
  })
  
  observeEvent(input$map_clicked_data, {
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
    df <- df[, colnames(df) %in% c(input$cb_selected_cols, "Flag", "Country"), with = FALSE]
    
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
      # columns_list <- c(
      #   columns_list, 
      #   sapply(column_choices$`Molecular testing`, simplify = FALSE, USE.NAMES = TRUE, function(x) {
      #     colDef(#class = "my-col"
      #       style = function(value) {
      #         list(`background-color` = "red")
      #       }
      #     )
      #   })
      # )
      
      columns_list <- columns_list[names(columns_list) %in% c(colnames(df), "Flag", "Country")]
    } else {
      testing_df_cols <- testing_cols[testing_cols %in% colnames(df)]
      df <- lapply(na.omit(unique(df[[input$rb_group]])), function(category) {
        ds <- lapply(testing_df_cols, function(col) {
          df2 <- df[get(input$rb_group) == category, ]
          df2[, list(get_table(df2[[col]]))]
        })

        cbind.data.frame(
          category,
          ds
        )
      }) %>%
        rbindlist(use.names = FALSE)
      
      colnames(df) <- c(input$rb_group, testing_df_cols)
      
      columns_list <- list(
        `Does the country have a policy that guides Covid-19 testing strategy?` = colDef(cell = function(value, index) {
          sparkline(df[["Does the country have a policy that guides Covid-19 testing strategy?"]][[index]], 
                    sliceColors = c("#cbcbcb", "#cd4651", "#44abb6"), 
                    type = "pie", height = "35px")
        }),
        
        `Is molecular testing registered for use in country?` = colDef(cell = function(value, index) {
          sparkline(df[["Is molecular testing registered for use in country?"]][[index]], 
                    sliceColors = c("#cbcbcb", "#cd4651", "#44abb6"), 
                    type = "pie", height = "35px")
        }),
        `Is molecular testing used to confirm a Covid-19 diagnosis?` = colDef(cell = function(value, index) {
          sparkline(df[["Is molecular testing used to confirm a Covid-19 diagnosis?"]][[index]], 
                    sliceColors = c("#cbcbcb", "#cd4651", "#44abb6"), 
                    type = "pie", height = "35px")
        }),
        
        `Are antigen rapid tests registered for use in country?` = colDef(cell = function(value, index) {
          sparkline(df[["Are antigen rapid tests registered for use in country?"]][[index]], 
                    sliceColors = c("#cbcbcb", "#cd4651", "#44abb6"), 
                    type = "pie", height = "35px")
        }),
        `Are antigen rapid tests used to confirm Covid-19 diagnosis?` = colDef(cell = function(value, index) {
          sparkline(df[["Are antigen rapid tests used to confirm Covid-19 diagnosis?"]][[index]], 
                    sliceColors = c("#cbcbcb", "#cd4651", "#44abb6"), 
                    type = "pie", height = "35px")
        }),
        `Are antigen rapid tests used for the testing of symptomatic patients?` = colDef(cell = function(value, index) {
          sparkline(df[["Are antigen rapid tests used for the testing of symptomatic patients?"]][[index]], 
                    sliceColors = c("#cbcbcb", "#cd4651", "#44abb6"), 
                    type = "pie", height = "35px")
        }),
        `Are antigen rapid tests used for the screening of asymptomatic patients?` = colDef(cell = function(value, index) {
          sparkline(df[["Are antigen rapid tests used for the screening of asymptomatic patients?"]][[index]], 
                    sliceColors = c("#cbcbcb", "#cd4651", "#44abb6"), 
                    type = "pie", height = "35px")
        }),
        `Are antigen rapid tests used for asymptomatic contacts of known positives (i.e., contact tracing)?` = colDef(cell = function(value, index) {
          sparkline(df[["Are antigen rapid tests used for asymptomatic contacts of known positives (i.e., contact tracing)?"]][[index]], 
                    sliceColors = c("#cbcbcb", "#cd4651", "#44abb6"), 
                    type = "pie", height = "35px")
        }),
        `Are antigen rapid tests used for testing of health care workers / front line staff?` = colDef(cell = function(value, index) {
          sparkline(df[["Are antigen rapid tests used for testing of health care workers / front line staff?"]][[index]], 
                    sliceColors = c("#cbcbcb", "#cd4651", "#44abb6"), 
                    type = "pie", height = "35px")
        }),
        `Are antigen rapid tests used for testing at borders / points of entry?` = colDef(cell = function(value, index) {
          sparkline(df[["Are antigen rapid tests used for testing at borders / points of entry?"]][[index]], 
                    sliceColors = c("#cbcbcb", "#cd4651", "#44abb6"), 
                    type = "pie", height = "35px")
        }),
        `Are antigen rapid tests used for testing at schools / workplaces?` = colDef(cell = function(value, index) {
          sparkline(df[["Are antigen rapid tests used for testing at schools / workplaces?"]][[index]], 
                    sliceColors = c("#cbcbcb", "#cd4651", "#44abb6"), 
                    type = "pie", height = "35px")
        }),
        `Are antigen rapid tests used for testing for non covid-19 hospitalized patients (e.g., scheduled or elective surgery)?` = colDef(cell = function(value, index) {
          sparkline(df[["Are antigen rapid tests used for testing for non covid-19 hospitalized patients (e.g., scheduled or elective surgery)?"]][[index]], 
                    sliceColors = c("#cbcbcb", "#cd4651", "#44abb6"), 
                    type = "pie", height = "35px")
        }),
        `Who is allowed to use the Ag-RDTs (only health workers etc)?` = colDef(cell = function(value, index) {
          sparkline(df[["Who is allowed to use the Ag-RDTs (only health workers etc)?"]][[index]], 
                    sliceColors = c("#cbcbcb", "#cd4651", "#44abb6"), 
                    type = "pie", height = "35px")
        }),
        
        `Are antibody rapid tests registered for use in country?` = colDef(cell = function(value, index) {
          sparkline(df[["Are antibody rapid tests registered for use in country?"]][[index]], 
                    sliceColors = c("#cbcbcb", "#cd4651", "#44abb6"), 
                    type = "pie", height = "35px")
        }),
        `Are antibody rapid tests used to confirm a Covid-19 diagnosis?` = colDef(cell = function(value, index) {
          sparkline(df[["Are antibody rapid tests used to confirm a Covid-19 diagnosis?"]][[index]], 
                    sliceColors = c("#cbcbcb", "#cd4651", "#44abb6"), 
                    type = "pie", height = "35px")
        }),
        `Are antibody rapid tests used for serosurveillance studies of Covid-19?` = colDef(cell = function(value, index) {
          sparkline(df[["Are antibody rapid tests used for serosurveillance studies of Covid-19?"]][[index]], 
                    sliceColors = c("#cbcbcb", "#cd4651", "#44abb6"), 
                    type = "pie", height = "35px")
        })
      )
      
      columns_list <- columns_list[names(columns_list) %in% colnames(df)]
    }
    
    # Column group list
    molecular_testing <- colnames(df)[colnames(df) %in% c("Is molecular testing registered for use in country?",
                                                          "Is molecular testing used to confirm a Covid-19 diagnosis?")]
    
    antigen_testing <- colnames(df)[colnames(df) %in% c("Are antigen rapid tests registered for use in country?",
                                                        "Are antigen rapid tests used to confirm Covid-19 diagnosis?",
                                                        "Are antigen rapid tests used for the testing of symptomatic patients?",
                                                        "Are antigen rapid tests used for the screening of asymptomatic patients?",
                                                        "Are antigen rapid tests used for asymptomatic contacts of known positives (i.e., contact tracing)?",
                                                        "Are antigen rapid tests used for testing of health care workers / front line staff?",
                                                        "Are antigen rapid tests used for testing at borders / points of entry?",
                                                        "Are antigen rapid tests used for testing at schools / workplaces?",
                                                        "Are antigen rapid tests used for testing for non covid-19 hospitalized patients (e.g., scheduled or elective surgery)?",
                                                        "Who is allowed to use the Ag-RDTs (only health workers etc)?")]
    
    antibody_testing <- colnames(df)[colnames(df) %in% c("Are antibody rapid tests registered for use in country?",
                                                         "Are antibody rapid tests used to confirm a Covid-19 diagnosis?",
                                                         "Are antibody rapid tests used for serosurveillance studies of Covid-19?")]
    
    columnGroups_list <- list(
      if (length(molecular_testing) > 0) {
        colGroup(name = "Molecular testing", columns = molecular_testing)
      },
      
      if (length(antigen_testing) > 0) {
        colGroup(name = "Antigen rapid tests", columns = antigen_testing)
      },
      
      if (length(antibody_testing) > 0) {
        colGroup(name = "Antibody rapid tests", columns = antibody_testing)
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
      range_filter_cols <- c("Continent", "Income", "Does the country have a policy that guides Covid-19 testing strategy?", testing_cols)
      
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
      value <- "Is molecular testing registered for use in country?"
    } else if (input$slt_category == "Antigen RDT") {
      value <- "Are antigen rapid tests registered for use in country?"
    } else if (input$slt_category == "Antibody RDT") {
      value <- "Are antibody rapid tests registered for use in country?"
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
    
    #label <- get_label(df[[value]])
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
    df[, value := value_lookup[ifelse(is.na(value), "No data", value)]]

    selected_test_cols <- switch (input$slt_category,
      `Molecular Test` = column_choices$`Molecular testing`,
      `Antigen RDT` = column_choices$`Antigen testing`,
      `Antibody RDT` = column_choices$`Antibody testing`
    )
    # selected_test_cols <- setdiff(
    #   selected_test_cols, 
    #   c("Is molecular testing registered for use in country?", 
    #     "Are antibody rapid tests registered for use in country?",
    #     "Are antigen rapid tests registered for use in country?")
    # )
    
    df %>%
      e_charts(name, dispose = FALSE) %>% 
      e_map_("value", roam = input$i_roam, bind = "updated", zoom = 1.20, center = c(0, 10),
             selectedMode = "multiple",
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
        # splitList = list(
        #   list(min = 1, max = 1),
        #   list(min = 2, max = 2),
        #   list(min = 3, max = 3)
        # )
        # splitList = split_list(r_outcome_info()$split_list),
        # precision = if (r_outcome_info()$split_list == "tiny") 1 else 0
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
          
          //'</h4>' + '%s' + '<span style=\"float: right;\">' + '<b>' + value + '</b>' + 
          return(params.name + '</span>' + '<br>' + '<span>' + '<ul>' + list + '</ul>' + '</span>')
        }
      ", paste0(selected_test_cols, collapse = "|"), value)), 
                position = JS("function (pos, params, dom, rect, size) {
                  // tooltip will be fixed on the right if mouse hovering on the left,
                  // and on the left if hovering on the right.
                  var obj = {top: 60};
                  obj[['left', 'right'][+(pos[0] < size.viewSize[0] / 2)]] = 5;
                  return obj;
                }"),
                backgroundColor = 'rgba(255, 255, 255, 0.6)'
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
            category,
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
}

