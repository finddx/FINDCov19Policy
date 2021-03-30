coldef_sparkline <- function(col) {
  colDef(cell = function(value, index) {
    sparkline(df[[col]], 
              sliceColors = c("#cbcbcb", "#cd4651", "#44abb6"), 
              type = "pie")
  })
}
