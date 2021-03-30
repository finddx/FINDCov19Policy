get_table <- function(value) {
  list(
    c(
      sum(is.na(value) | value == "No data", na.rm = TRUE),
      sum(value == "No", na.rm = TRUE),
      sum(value == "Yes", na.rm = TRUE)
    )
  )
}
