get_label <- function(x) {
  vals <- unique(x)
  
  lapply(seq_along(vals), function(i) {
    val <- ifelse(is.na(vals[i]), "NA", vals[i])
    
    list(min = i, max = i, label = val)
  })
}
