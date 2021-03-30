# create a character vector of shiny inputs
shinyInput = function(FUN, len = NULL, id, ...) {
  if (!is.null(len)) {
    inputs = character(len)
    
    for (i in seq_len(len)) {
      inputs[i] = as.character(div(FUN(paste0(id, i), label = NULL, ...)), style = "margin-right: 8px;")
    }
  } else {
    inputs = as.character(div(FUN(id, label = NULL, ...)), style = "margin-right: 8px;")
  }
  inputs
}