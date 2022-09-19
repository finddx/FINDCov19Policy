parse_excel_date <- function(x) {
  out <- ifelse(stringr::str_detect(x, pattern = "[0-9]{5}"), 
         as.Date(as.numeric(x), origin = "1899-12-30"),
         lubridate::as_date(x))
  
  lubridate::as_date(out)
}
