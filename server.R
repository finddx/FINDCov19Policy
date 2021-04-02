function(input, output, session) {
  selected <- reactive(getReactableState("tbl_country_list", "selected"))

  observe( {
    print(input$map_clicked_data)
  })
  
  # Render table --------------------------------
  output$tbl_country_list <- reactable::renderReactable( {
    df <- dx_policy
    
    # Select only cols to show
    df <- df[, colnames(df) %in% c(input$cb_selected_cols, "Flag", "Country"), with = FALSE]
    
    # Select only rows to show
    if (input$cb_show_data) {
      na_rows <- df[, rowSums(sapply(.SD, is.na)), .SDcols = testing_cols[testing_cols %in% colnames(df)]]
      df <- df[na_rows == 0]
    }
    
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
        })
      )
      
      columns_list <- columns_list[names(columns_list) %in% colnames(df)]
    }
    
    # Column group list
    molecular_testing <- colnames(df)[colnames(df) %in% c("Is molecular testing registered for use in country?",
                                                          "Is molecular testing used to confirm a Covid-19 diagnosis?")]
    
    antibody_testing <- colnames(df)[colnames(df) %in% c("Are antibody rapid tests registered for use in country?",
                                                         "Are antibody rapid tests used to confirm a Covid-19 diagnosis?",
                                                         "Are antibody rapid tests used for serosurveillance studies of Covid-19?")]
    
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
    
    columnGroups_list <- list(
      if (length(molecular_testing) > 0) {
        colGroup(name = "Molecular testing", columns = molecular_testing)
      },
      
      if (length(antibody_testing) > 0) {
        colGroup(name = "Antibody rapid tests", columns = antibody_testing)
      },
      
      if (length(antigen_testing) > 0) {
        colGroup(name = "Antigen rapid tests", columns = antigen_testing)
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

  output$map <- renderEcharts4r( {
    req(input$slt_category)
    
    if (input$slt_category == "Molecular Test") {
      category <- "Is molecular testing registered for use in country?"
    } else if (input$slt_category == "Antigen RDT") {
      category <- "Are antigen rapid tests registered for use in country?"
    } else if (input$slt_category == "Antibody RDT") {
      category <- "Are antibody rapid tests registered for use in country?"
    }
    
    theme <- "grey"
    
    df <- copy(data_map)
    df <- df[Country != "Kosovo"]
    df <- e_country_names(df, iso2c, unit)

    colors <- c(
      if (NA %in% df[[category]]) {
        "#cbcbcb"
      },
      if ("No data" %in% df[[category]]) {
        "#cbcbcb"
      },
      if ("No" %in% df[[category]]) {
        "#cd4651"
      },
      if ("Yes" %in% df[[category]]) {
        "#44abb6"
      }
    )
    
    #label <- get_label(df[[category]])
    label <- list(
      if (NA %in% df[[category]]) {
        list(min = 0, max = 0, label = "NA") 
      },
      if ("No data" %in% df[[category]]) {
        list(min = 1, max = 1, label = "No data")
      },
      if ("No" %in% df[[category]]) {
        list(min = 2, max = 2, label = "No")
      },
      if ("Yes" %in% df[[category]]) {
        list(min = 3, max = 3, label = "Yes")
      }
    )
    label[sapply(label, is.null)] <- NULL
    
    df$category <- df[[category]]
    df[, category := value_lookup[ifelse(is.na(category), "NA", category)]]
    
    df %>%
      e_charts(unit, dispose = FALSE) %>% 
      e_map_("category", roam = input$i_roam, bind = "updated", zoom = 1.20, center = c(0, 10)) %>%
      e_visual_map_(
        "category",
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
          return('%s' + '<br>' + params.name + '<span style=\"float: right;\">' + '<b>' + value + '</b>' + '</span>')
        }
      ", category)))
  })
  
  # Download selected rows ----------------------
  output$lnk_download_selected <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      df <- dx_policy[selected(), colnames(dx_policy) %in% input$cb_selected_cols, with = FALSE]
      
      if (input$cb_show_data) {
        na_rows <- df[, rowSums(sapply(.SD, is.na)), .SDcols = testing_cols[testing_cols %in% colnames(df)]]
        df <- df[na_rows == 0]
      }
      
      fwrite(x = df, file = file)
    }
  )
  
  # output$tbl_country_list <- renderDT(server = TRUE, {
  #   
  #   df <- dx_policy
  # 
  #   flag_render <- JS("
  #     function(data, type, row) {
  #       if (type === 'display') {
  #         var x = row[1] + data;
  #         return x;
  #       } else {
  #         return data;
  #       }
  #     }
  #   ")
  #   
  #   long_cols_pos <- which(colnames(df) %in% names(long_cols))
  #   
  #   table_container <- htmltools::withTags(table(
  #     class = 'display',
  #     thead(
  #       tr(
  #         th(rowspan = 2, HTML(select_colname)),
  #         th(rowspan = 2, 'Flag'),
  #         th(rowspan = 2, 'Country'),
  #         th(rowspan = 2, "Continent"),
  #         th(rowspan = 2, 'Region'),
  #         th(rowspan = 2, 'Income'),
  #         th(rowspan = 2, 'Date of last update'),
  #         th(rowspan = 2, 'Does the country have a policy that guides covid-19 testing strategy?', style = "border-right: solid 1px;"),
  #         
  #         th(colspan = 2, 'Molecular testing', style = "border-right: solid 1px;"),
  #         th(colspan = 3, 'Antibody rapid tests', style = "border-right: solid 1px;"),
  #         th(colspan = 3, 'Antigen rapid tests', style = "border-right: solid 1px;"),
  #         
  #         th(rowspan = 2, "Policy Links")
  #       ),
  #       tr(
  #         th('Registered for use'),
  #         th('Used to confirm diagnosis', style = "border-right: solid 1px;"),
  #         
  #         th('Registered for use'),
  #         th('Used to confirm diagnosis'),
  #         th('Used for serosurveillance', style = "border-right: solid 1px;"),
  #         
  #         th('Registered for use'),
  #         th('Used to confirm diagnosis'),
  #         th('Used for the screening of symptomatic patients', style = "border-right: solid 1px;"),
  #         
  #         th('Used for the screening of asymptomatic patients'),
  #         th('Are antigen rapid tests used for asymptomatic contacts of known positives (i.e., contact tracing)?'),
  #         th('Are antigen rapid tests used for testing of health care workers / front line staff?'),
  #         th('Are antigen rapid tests used for testing at borders /points of entry?'),
  #         th('Are antigen rapid tests used for testing at schools / workplaces?'),
  #         th('Are antigen rapid tests used for testing for non covid-19 hospitalized patients (e.g., scheduled or elective surgery)?'),
  #         th('Who is allowed to use the Ag-RDTs (only health workers etc)?')
  #       )
  #     )
  #   ))
  #   
  #   #cols_to_show <- colnames(df) %in% c("Flag", select_colname, input$cb_selected_cols)
  #   
  #   # Table
  #   DT::datatable(df,#[, cols_to_show, with = FALSE],
  #                 escape = FALSE,
  #                 rownames = FALSE,
  #                 class = "display compact stripe",
  #                 selection = "none",
  #                 filter = "top",
  #                 container = table_container,
  #                 options = list(
  #                   #dom = "t",
  #                   #pageLength = 15000,
  #                   #scrollX = TRUE,
  #                   #scrollY = "500px",
  #                   autoWidth = TRUE,
  #                   columnDefs = list(
  #                     list(targets = 0, width = '20px', orderable = FALSE, className = 'dt-center'), # Select column
  #                     list(targets = 1, visible = FALSE), # Flag
  #                     #list(width = '150px', targets = c(4:ncol(df) - 1)),
  #                     list(targets = 2, width = "200px", render = flag_render)
  #                     # list(targets = long_cols_pos - 1, width = "600px")
  #                   ),
  #                   order = list(
  #                     list(2, 'asc')
  #                   ),
  #                   # #fixedColumns = list(leftColumns = 1),
  #                   #
  #                   # Row selection checkbox
  #                   preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
  #                   drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
  # 
  #                 ),
  #                 extensions = c("FixedColumns")) %>%
  #     formatStyle(columns = c(1, 2), width = '20px') %>%
  #     formatStyle(columns = c("Date of last update", "Country", "Continent", "Region", "Income"), "white-space" = "nowrap")
  # })
  # 
  # proxy <- dataTableProxy("tbl_country_list", session = session)

  # Filter columns ------------------------------
  # observeEvent(input$cb_selected_cols, {
  #   df <- dx_policy
  #   cols_to_show <- which(colnames(df) %in% c(input$cb_selected_cols, select_colname)) - 1
  #   
  #   DT::showCols(proxy, show = cols_to_show, reset = TRUE)
  # })

  # Select all rows -----------------------------
  # observeEvent(input$select_all, {
  #   req(!is.null(input$select_all))
  #   
  #   df <- dx_policy
  #   if (input$select_all) {
  #     df[, (select_colname) := shinyInput(checkboxInput, nrow(dx_policy), 'tbl_selection_', value = TRUE)]
  #   } else {
  #     df[, (select_colname) := shinyInput(checkboxInput, nrow(dx_policy), 'tbl_selection_', value = FALSE)]
  #   }
  # 
  #   DT::replaceData(proxy, df, resetPaging = FALSE, clearSelection = FALSE, rownames = FALSE)
  # })
  
  # # Row selected
  # observeEvent(input$tbl_country_list_rows_selected, {
  #   print(input$tbl_country_list_rows_selected)
  #   print(isolate(rv$previous_row_selection))
  #   
  #   if (length(input$tbl_country_list_rows_selected) > length(isolate(rv$previous_row_selection))) {
  #     selected_row <- input$tbl_country_list_rows_selected[!input$tbl_country_list_rows_selected %in% isolate(rv$previous_row_selection)]  
  #   } else {
  #     selected_row <- isolate(rv$previous_row_selection)[!isolate(rv$previous_row_selection) %in% input$tbl_country_list_rows_selected]
  #   }
  #   
  #   print(selected_row)
  #   rv$previous_row_selection <- input$tbl_country_list_rows_selected
  # })
}

