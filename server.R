function(input, output, session) {
  # Diagnostic
  callModule(mod_map_server, id = "mod_map_diagnostic", session = session, 
             column_choices = dx_column_choices,
             questions_lkp = dx_questions_lkp,
             main_cols = dx_testing_cols,
             data_map = dx_data_map)
  
  callModule(mod_policy_table_server, id = "mod_table_diagnostic", session = session,
             data = dx_policy,
             main_cols = dx_testing_cols,
             column_choices = dx_column_choices,
             add_policy_testing = TRUE)
  
  # Treatment
  callModule(mod_map_server, id = "mod_map_treatment", session = session,
             column_choices = tx_column_choices,
             questions_lkp = tx_questions_lkp,
             main_cols = tx_treatment_cols,
             data_map = tx_data_map)
  
  callModule(mod_policy_table_server, id = "mod_table_treatment", session = session,
             data = tx_policy,
             main_cols = tx_treatment_cols,
             column_choices = tx_column_choices,
             add_policy_testing = FALSE)
}
