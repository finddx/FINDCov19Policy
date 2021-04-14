build_filter_range <- function(col) {
  values <- na.omit(unique(col))
  
  htmlwidgets::JS(sprintf("
    function inputFilter(filter) {
        var _onChange = filter.onChange;
        return React.createElement(
          'div',
          null,
          React.createElement(
            MaterialUI.Select,
            {
              children: [
                          React.createElement('option', {value: ''}, ''),
                          %s
              ],
              native: true,
              onChange: function onChange(event, newValue) {return _onChange(event.target.value)}
            }
          )
        )
      }
  ", paste0(sapply(values, function(x) {
    sprintf(
      "React.createElement('option', {value: '%s'}, '%s')",
      x, x
    )
  }), collapse = ",")
  ))
}
