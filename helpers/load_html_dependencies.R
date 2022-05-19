load_html_dependencies <- function() {
  material_dep <- htmltools::htmlDependency(
    name = "material-ui",
    version = "4.6.1",
    src = c(href = "https://unpkg.com/@material-ui/core/umd/"),
    script = "material-ui.production.min.js"
  )
  
  tags$head(
    # custom CSS
    tags$link(href = "flag-icon.min.css", rel = "stylesheet"),
    tags$link(href = "imperial-urw.css", rel = "stylesheet"),
    tags$link(href = "style.css", rel = "stylesheet"),
    
    # Font
    tags$link(href="https://fonts.googleapis.com/css2?family=Roboto:300"),
    
    reactR::html_dependency_react(),
    reactR::html_dependency_reacttools(),
    htmlwidgets::getDependency("reactable","reactable"),
    material_dep,
    tags$style("
        .rt-thead.-filters .rt-tr {align-items: flex-end; height: 60px;}
        .rt-thead.-filters .rt-th {overflow: visible;}
        menuitem {display: block;}

        .dropdown-menu {
          z-index: 1000001 !important;
        }

        /* Underline From Left */
        .hvr-underline-from-left {
          display: inline-block;
          vertical-align: middle;
          -webkit-transform: perspective(1px) translateZ(0);
          transform: perspective(1px) translateZ(0);
          box-shadow: 0 0 1px rgba(0, 0, 0, 0);
          position: relative;
          overflow: hidden;
        }
        .hvr-underline-from-left:before {
          content: '';
          position: absolute;
          z-index: -1;
          left: 0;
          right: 100%;
          bottom: 0;
          background: #5a2259;
          height: 4px;
          -webkit-transition-property: right;
          transition-property: right;
          -webkit-transition-duration: 0.3s;
          transition-duration: 0.3s;
          -webkit-transition-timing-function: ease-out;
          transition-timing-function: ease-out;
        }
        .hvr-underline-from-left:hover:before, .hvr-underline-from-left:focus:before, .hvr-underline-from-left:active:before {
          right: 0;
        }
      "),
    
    tags$script(HTML("
        function filterRange(filter, rows) {
          return rows.filter(function(row) {
            // Don't filter on aggregated cells
            if (row._subRows) {
              return true;
            }

            if (filter.value == '') {
              return true;
            }
            return row[filter.id] == filter.value;
          })
        }

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
                            React.createElement('option', {value: 'Yes'}, 'Yes'),
                            React.createElement('option', {value: 'No'}, 'No'),
                            React.createElement('option', {value: 'No data'}, 'No data')
                ],
                native: true,
                onChange: function onChange(event, newValue) {return _onChange(event.target.value)}
              }
            )
          )
        }
      "))
  )
}
