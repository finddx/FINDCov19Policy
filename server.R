function(input, output, session) {
  callModule(mod_map_server, id = "mod_map", session = session)
  callModule(mod_policy_table_server, id = "mod_table", session = session)
}
