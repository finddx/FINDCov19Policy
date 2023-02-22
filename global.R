rm(list = ls())

library(shiny)
library(tablerDash)
library(reactable)
library(echarts4r)
library(leaflet)

library(shinyWidgets)
library(DT)
library(readxl)
library(data.table)
library(stringr)
library(countrycode)
library(lubridate)
library(purrr)
library(sparkline)
library(r2d3)
library(openxlsx)
library(urltools)
library(cachem)
library(writexl)

# Set locale to English for date formatting
#Sys.setlocale("LC_ALL","English")
Sys.setlocale("LC_TIME", "C")

# Read helper functions
helper_files <- dir("helpers", full.names = TRUE)
lapply(helper_files, source)

# Read modules
module_files <- dir("modules", full.names = TRUE)
lapply(module_files, source)

# World Bank classification ---------------------
wb_classification <- read_world_bank_classification()

# Read dx policy --------------------------------
policy_file_path <- file.path("data", "Policy_Mapping.xlsx")
dx_policy <- read_dx_policy(policy_file_path)

dx_policy$`Policy Links 21` <- "https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups"

# Split dataset ---------------------------------
tx_only_cols <- c(
  "Included in generic voluntary license territory for Nirmatrelvir/Ritonavir generics?",
  "Included in generic voluntary license territory for Molnupiravir generics?",
  "Included in originator access agreement with ACT-A for Nirmatrelvir/Ritonavir?",
  "Included in originator access agreement with ACT-A for Molnupiravir?",
  "Policy Links 17", "Policy Links 18",	"Policy Links 19", "Policy Links 20", "Policy Links 21"
)
common_cols <- c("Flag", "Country", "Continent", "Income", "ISO", "Region", "Date of last update", "iso2c")
merge_cols <- c(common_cols, "Policy Links 17", "Policy Links 18",	"Policy Links 19", "Policy Links 20", "Policy Links 21")
tx_cols <- c(common_cols, tx_only_cols)

tx_policy <- read_tx_policy(file.path("data", "Therapeutics.xlsx"))
tx_policy <- merge(x = tx_policy, y = dx_policy[, ..merge_cols], by = "ISO")
setcolorder(tx_policy, tx_cols)

# N/A to "No data"
therapeutics_cols <- c("Included in generic voluntary license territory for Nirmatrelvir/Ritonavir generics?",
                       "Included in generic voluntary license territory for Molnupiravir generics?",
                       "Included in originator access agreement with ACT-A for Nirmatrelvir/Ritonavir?",
                       "Included in originator access agreement with ACT-A for Molnupiravir?")
tx_policy[, (therapeutics_cols) := lapply(.SD, function(x) ifelse(x %in% "N/A", "Not applicable", x)), .SDcols = therapeutics_cols]

# Change public sector
tx_policy[, (therapeutics_cols) := lapply(.SD, function(x) ifelse(x %in% "Yes (public sector)", "Yes (public sector only)", x)), 
          .SDcols = therapeutics_cols]

# Split xlsx ------------------------------------
content_path <- "content"
if (!dir.exists(content_path)) {
  dir.create(content_path)
}

writexl::write_xlsx(x = dx_policy[, -1], path = file.path(content_path, "dx_policy.xlsx"))
writexl::write_xlsx(x = tx_policy[, -1], path = file.path(content_path, "tx_policy.xlsx"))

# Data map
dx_data_map <- copy(dx_policy)
tx_data_map <- copy(tx_policy)

dx_cols <- names(dx_policy)[!names(dx_policy) %in% tx_only_cols]
dx_policy <- dx_policy[, ..dx_cols]

# Remove unnecessary columns --------------------
dx_policy[, `:=`(
  ISO = NULL,
  iso2c = NULL,
  Region = NULL
)]

# Global variables ------------------------------
public_cols <- setdiff(colnames(dx_policy), c("Flag"))

dx_default_cols <- c("Country",
                     "Continent",
                     "Income",
                     "COVID-19 testing strategy available",
                     "Molecular test registered in country",
                     "Molecular test used to confirm COVID-19 diagnosis",
                     "Antibody RDTs registered in country",
                     "Antibody RDTs used to confirm COVID-19 diagnosis",
                     "Antibody RDTs used for serosurveillance studies of COVID-19",
                     "Antigen RDTs registered in country",
                     "Antigen RDTs used to confirm COVID-19 diagnosis",
                     "Antigen RDTs used for testing symptomatic cases",
                     
                     "Does the country have a policy guiding COVID-19 self-testing",
                     "Self tests registered for use in country",
                     "Self tests used in the screening of symptomatic cases")

tx_default_cols <- c("Country",
                     "Continent",
                     "Income",
                     "Included in generic voluntary license territory for Molnupiravir generics?",
                     "Included in generic voluntary license territory for Nirmatrelvir/Ritonavir generics?",
                     "Included in originator access agreement with ACT-A for Molnupiravir?",
                     "Included in originator access agreement with ACT-A for Nirmatrelvir/Ritonavir?")

dx_column_choices <- list(
  General = c(
    "Country",
    "Continent",
    "Income",
    "Date of last update",
    "COVID-19 testing strategy available",
    "Choice of molecular or rapid tests in order of priority",
    "Policy links"
  ),
  `Molecular Test` = c(
    "Molecular test registered in country",
    "Molecular test used to confirm COVID-19 diagnosis"
  ),
  `Antibody RDT` = c(
    "Antibody RDTs registered in country",
    "Antibody RDTs used to confirm COVID-19 diagnosis",
    "Antibody RDTs used for serosurveillance studies of COVID-19"
  ),
  `Professional Use Antigen RDT` = c(
    "Antigen RDTs registered in country",
    "Antigen RDTs used to confirm COVID-19 diagnosis",
    "Antigen RDTs used for testing symptomatic cases",
    "Antigen RDTs used for testing asymptomatic populations"
  ),
  `Self-test Antigen RDT` = c(
    "Does the country have a policy guiding COVID-19 self-testing",
    "Self tests registered for use in country",
    "Self tests used in the screening of symptomatic cases",
    "Self tests used in the screening of asymptomatic populations"
  )
)

# TEMP
column_choices <- dx_column_choices

tx_column_choices <- list(
  General = c(
    "Country",
    "Continent",
    "Income",
    "Date of last update"
  ),
  `Nirmatrelvir/Ritonavir` = c(
    "Included in generic voluntary license territory for Nirmatrelvir/Ritonavir generics?",
    "Included in originator access agreement with ACT-A for Nirmatrelvir/Ritonavir?"
  ),
  `Molnupiravir` = c(
    "Included in generic voluntary license territory for Molnupiravir generics?",
    "Included in originator access agreement with ACT-A for Molnupiravir?"
  )
)

dx_testing_cols <- c("COVID-19 testing strategy available",
                  
                     "Molecular test registered in country",
                     "Molecular test used to confirm COVID-19 diagnosis",
                     
                     "Antigen RDTs registered in country",
                     "Antigen RDTs used to confirm COVID-19 diagnosis",
                     "Antigen RDTs used for testing symptomatic cases",
                     "Antigen RDTs used for testing asymptomatic populations",
                     
                     "Antibody RDTs registered in country",
                     "Antibody RDTs used to confirm COVID-19 diagnosis",
                     "Antibody RDTs used for serosurveillance studies of COVID-19",
                     
                     "Does the country have a policy guiding COVID-19 self-testing",
                     "Self tests registered for use in country",
                     "Self tests used in the screening of symptomatic cases",
                     "Self tests used in the screening of asymptomatic populations")

tx_treatment_cols <- c(
  "Included in generic voluntary license territory for Molnupiravir generics?",
  "Included in generic voluntary license territory for Nirmatrelvir/Ritonavir generics?",
  "Included in originator access agreement with ACT-A for Molnupiravir?",
  "Included in originator access agreement with ACT-A for Nirmatrelvir/Ritonavir?"         
)

dx_questions_lkp <- list(
  `Molecular Test` = "Molecular test registered in country",
  `Antibody RDT` = "Antibody RDTs registered in country",
  `Professional Use Antigen RDT` = "Antigen RDTs registered in country",
  `Self-test Antigen RDT` = "Self tests registered for use in country"
)

tx_questions_lkp <- list(
  `Molnupiravir` = "Included in generic voluntary license territory for Molnupiravir generics?",
  `Nirmatrelvir/Ritonavir` = "Included in generic voluntary license territory for Nirmatrelvir/Ritonavir generics?"
)

dx_policy[, (dx_testing_cols) := lapply(.SD, function(x) {
  ifelse(x == "No Data", "No data", x)
}), .SDcols = dx_testing_cols]

dx_policy[, (dx_testing_cols) := lapply(.SD, function(x) {
  ifelse(x == "yes", "Yes", x)
}), .SDcols = dx_testing_cols]

setcolorder(dx_policy, c("Flag", "Country", "Continent", "Income", "Date of last update",
                         "COVID-19 testing strategy available",
                         dx_column_choices$`Molecular Test`,
                         dx_column_choices$`Professional Use Antigen RDT`,
                         dx_column_choices$`Antibody RDT`,
                         dx_column_choices$`Self-test Antigen RDT`
))

# Convert columns to factor/date ----------------
factor_cols <- public_cols[!public_cols %in% c("Policy links", "Date of last update")]
dx_policy[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]
dx_policy[, `Date of last update` := parse_excel_date(`Date of last update`)]
#dx_policy[, `Date of last update` := as_date(`Date of last update`)]

value_lookup <- c("NA" = 1, "No data" = 1, "No Data" = 1, "No, but used" = 2, "In the process of registration" = 2, "No" = 2, "Yes" = 3,
                  "Yes (public sector only)" = 4, "Yes (Access price)" = 3, "Yes (Tiered price)" = 4)
value_lookup2 <- c("NA" = 1, "No data" = 1, "No Data" = 1, "Data available" = 2)

# Map to use
#' Source: https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json
#' Modifications: Somaliland merged into Somalia, Northern Cyprus merged into Cyprus
geojson <- jsonlite::read_json(file.path("map", "countries.geo.json"))

# Store cache persistently
shinyOptions(cache = cachem::cache_disk(file.path("cache")))
