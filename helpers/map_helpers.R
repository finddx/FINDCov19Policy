get_map_colors <- function(x) {
  c(
    if ("No data" %in% x) {
      "#cbcbcb"
    },
    if ("No" %in% x) {
      "#cd4651"
    },
    if ("Yes" %in% x) {
      "#44abb6"
    }
  )
}

get_map_labels <- function(x) {
  label <- list(
    if ("No data" %in% x) {
      list(min = 1, max = 1, label = "No data")
    },
    if ("No" %in% x) {
      list(min = 2, max = 2, label = "No")
    },
    if ("Yes" %in% x) {
      list(min = 3, max = 3, label = "Yes")
    }
  )
  label[sapply(label, is.null)] <- NULL
  label
}

get_recoded_countries <- function(name) {
  dplyr::recode(name,
                "Bahamas" = "The Bahamas",
                "Dominican Rep." = "Dominican Republic",
                "Tanzania" = "United Republic of Tanzania",
                "Eq. Guinea" = "Equatorial Guinea",
                "Timor-Leste" = "East Timor",
                "Solomon Is." = "Solomon Islands",
                "United States" = "United States of America",
                "Dem. Rep. Congo" = "Democratic Republic of the Congo",
                "Congo, Dem. Rep." = "Democratic Republic of the Congo",
                "Congo" = "Republic of the Congo",
                "W. Sahara" = "Western Sahara",
                "S. Sudan" = "South Sudan",
                "Korea" = "North Korea",
                "Dem. Rep. Korea" = "South Korea",
                "Guinea-Bissau" = "Guinea Bissau",
                "Serbia" = "Republic of Serbia",
                
                "Myanmar (Burma)" = "Myanmar",
                "Laos" = "Lao PDR",
                "Côte d’Ivoire" = "Ivory Coast",
                "Czech Rep." = "Czech Republic",
                "Eswatini" = "Swaziland",
                "Falkland Islands" = "Falkland Is.",
                "South Georgia & South Sandwich Islands" = "S. Geo. and S. Sandw. Is.",
                "French Southern Territories" = "French Southern and Antarctic Lands",
                "British Indian Ocean Territory" = "Br. Indian Ocean Ter.",
                "Bosnia and Herz." = "Bosnia and Herzegovina",
                "North Macedonia" = "Macedonia",
                "Heard & McDonald Islands" = "Heard I. and McDonald Is.",
                "Micronesia (Federated States of)" = "Micronesia",
                "Trinidad & Tobago" = "Trinidad and Tobago",
                "St. Vincent & Grenadines" = "St. Vin. and Gren.",
                "St. Lucia" = "Saint Lucia",
                "Antigua & Barbuda" = "Antigua",
                "U.S. Virgin Islands" = "U.S. Virgin Is.",
                "Faroe Islands" = "Faeroe Is.",
                "Åland Islands" = "Aland",
                "Central African Rep." = "Central African Republic")
}