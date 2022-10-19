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
wb_classification <- readxl::read_xls("data/WorldBank Classification.xls", skip = 4)
setDT(wb_classification)
wb_classification <- wb_classification[!is.na(`Income group`) & `Income group` != "x", .(Code, `Income group`)]
wb_classification[, Income := `Income group`]
wb_classification[, `Income group` := NULL]

# Read dx policy --------------------------------
policy_file_path <- "data/Policy_Mapping.xlsx"
dx_policy <- readxl::read_xlsx(policy_file_path)
setDT(dx_policy)
dx_policy[, `Date of last update` := as.character(`Date of last update`)]
dx_policy[, `Notes` := NULL]

# Remove extra spaces from colnames
names(dx_policy) <- str_replace_all(names(dx_policy), pattern = " +", replacement = " ")

# Covid case sensitivity
names(dx_policy) <- str_replace_all(names(dx_policy), pattern = regex("covid-19", ignore_case = TRUE), replacement = "COVID-19")

# Rename self-test questions
names(dx_policy) <- str_replace_all(names(dx_policy), pattern = regex("Are self[- ]tests?", ignore_case = TRUE), replacement = "Self tests")

# Simplify questions
setnames(dx_policy,
         old = c(
           # General
           "Does the country have a policy that guides COVID-19 testing strategy?",
           "What is the choice of test (i.e PCR or AgRDT) in order of priority?",
           
           # Molecular tests
           "Is molecular testing registered for use in country?",
           "Is molecular testing used to confirm a COVID-19 diagnosis?",
           "What is the policy recommendation for where to use PCR tests?",
           "Who are the trained operators allowed to administer the PCR test?",
           
           # Professional use Antigen
           "Are antigen rapid tests registered for use in country?",
           "Are antigen rapid tests used to confirm COVID-19 diagnosis?",
           "Can antigen rapid tests be used for the testing/screening of symptomatic cases?",
           "Can antigen rapid tests be used for the testing/screening of asymptomatic populations?",
           "What is the policy recommendation for where to use AgRDT tests?",
           "Who are the trained operators allowed to administer the Ag-RDT test?",

           # Antibody
           "Are antibody rapid tests registered for use in country?",
           "Are antibody rapid tests used to confirm a COVID-19 diagnosis?",
           "Are antibody rapid tests used for serosurveillance studies of COVID-19?",
           
           # Self Tests
           "Does the country have a policy guiding COVID-19 self-testing?",
           "Self tests registered for use in country?",
           "Can self-tests be used in the testing/screening of symptomatic patients?",
           "Can self-tests used in the testing/screening of asymptomatic populations?"
         ),
         new = c(
           # General
           "COVID-19 testing strategy available",
           "Choice of molecular or rapid tests in order of priority",
           
           # Molecular tests
           "Molecular test registered in country",
           "Molecular test used to confirm COVID-19 diagnosis",
           "Policy recommendation on where to use molecular tests",
           "Trained operators administering molecular tests",
           
           # Professional use Antigen
           "Antigen RDTs registered in country",
           "Antigen RDTs used to confirm COVID-19 diagnosis",
           "Antigen RDTs used for testing symptomatic cases",
           "Antigen RDTs used for testing asymptomatic populations",
           "Policy recommendation on where to use antigen rapid tests",
           "Trained operators administering antigen rapid tests",

           # Antibody
           "Antibody RDTs registered in country",
           "Antibody RDTs used to confirm COVID-19 diagnosis",
           "Antibody RDTs used for serosurveillance studies of COVID-19",
           
           # Self Tests
           "Does the country have a policy guiding COVID-19 self-testing",
           "Self tests registered for use in country",
           "Self tests used in the screening of symptomatic cases",
           "Self tests used in the screening of asymptomatic populations"
         ))

# Remove extra whitespace from policy links
policy_links_cols <- str_subset(names(dx_policy), pattern = "^Policy Links [0-9]+")
dx_policy[, (policy_links_cols) := lapply(.SD, str_remove_all, pattern = "\\r\\n"), .SDcols = policy_links_cols]

# Remove invalid links
dx_policy[, (policy_links_cols) := lapply(.SD, function(x) ifelse(is_valid_url(x), x, NA_character_)), .SDcols = policy_links_cols]

# Keep only the link part
dx_policy[, (policy_links_cols) := lapply(.SD, get_url), .SDcols = policy_links_cols]

# Turn policy links into links ------------------
setnames(dx_policy, "Policy Links", "Policy links")

## Use <br> for linebreaks
# dx_policy[, `Policy links` := str_remove_all(`Policy links`, pattern = "<br>NA")]
#dx_policy[, `Policy links` := str_remove_all(`Policy links`, pattern = "\\n")]
dx_policy[, `Policy links` := str_replace_all(`Policy links`, pattern = "(\\r)*\\n", replacement = "<br>")]
dx_policy[, `Policy links` := str_replace_all(`Policy links`, pattern = "(http(s)?://[^<]+)", replacement = "<a href='\\1'>\\1</a>")]

# Merge continent -------------------------------
codelist <- setNames(countrycode::codelist[, c("iso2c", "iso3c", "continent", "iso.name.en")], c("iso2c", "iso3c", "Continent", "iso.name.en"))
codelist <- codelist[!is.na(codelist$iso.name.en), ]

dx_policy <- merge(
  x = dx_policy,
  y = codelist,
  by.x = "ISO",
  by.y = "iso3c",
  all.x = TRUE,
  all.y = TRUE
)

dx_policy[, Country := ifelse(is.na(Country), iso.name.en, Country)]
dx_policy[, iso.name.en := NULL]

dx_policy <- merge(
  x = dx_policy,
  y = wb_classification,
  by.x = "ISO",
  by.y = "Code",
  all.x = TRUE
)

# Add flags -------------------------------------
dx_policy[Country %in% "Tanzania", "iso2c"] <- "tz"
dx_policy[, `Flag` := paste0(
  ifelse(!is.na(dx_policy$iso2c), sprintf('<span class="flag-icon flag-icon-%s" style="margin-right: 5px;"></span>', tolower(dx_policy$iso2c)), "")
)]

# Add selection checkbox column -----------------
# select_colname <- shinyInput(checkboxInput, id = 'select_all', value = TRUE)
# dx_policy[, (select_colname) := shinyInput(checkboxInput, nrow(dx_policy), 'tbl_selection_', value = TRUE)]
setcolorder(dx_policy, c("Flag", "Country", "Continent", "Income", "Date of last update"))

data_map <- copy(dx_policy)

# Remove unnecessary columns --------------------
dx_policy[, `:=`(
  ISO = NULL,
  iso2c = NULL,
  Region = NULL
)]

# # Determine long cols ---------------------------
# col_char_no <- unlist(lapply(dx_policy, function(x) {
#   max(nchar(x), na.rm = TRUE)
# }))
# long_cols <- col_char_no[col_char_no > 500]

# Global variables ------------------------------
public_cols <- setdiff(colnames(dx_policy), c("Flag"))

default_cols <- c("Country",
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

column_choices <- list(
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

testing_cols <- c("COVID-19 testing strategy available",
                  
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

registration_questions <- list(
  `Molecular Test` = "Molecular test registered in country",
  `Antibody RDT` = "Antibody RDTs registered in country",
  `Professional Use Antigen RDT` = "Antigen RDTs registered in country",
  `Self-test Antigen RDT` = "Self tests registered for use in country"
)

dx_policy[, (testing_cols) := lapply(.SD, function(x) {
  ifelse(x == "No Data", "No data", x)
}), .SDcols = testing_cols]

dx_policy[, (testing_cols) := lapply(.SD, function(x) {
  ifelse(x == "yes", "Yes", x)
}), .SDcols = testing_cols]

setcolorder(dx_policy, c("Flag", "Country", "Continent", "Income", "Date of last update",
                         "COVID-19 testing strategy available",
                         column_choices$`Molecular Test`,
                         column_choices$`Professional Use Antigen RDT`,
                         column_choices$`Antibody RDT`,
                         column_choices$`Self-test Antigen RDT`
))

# Convert columns to factor/date ----------------
factor_cols <- public_cols[!public_cols %in% c("Policy links", "Date of last update")]
dx_policy[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]
dx_policy[, `Date of last update` := parse_excel_date(`Date of last update`)]
#dx_policy[, `Date of last update` := as_date(`Date of last update`)]

value_lookup <- c("NA" = 1, "No data" = 1, "No Data" = 1, "No, but used" = 2, "In the process of registration" = 2, "No" = 2, "Yes" = 3)
value_lookup2 <- c("NA" = 1, "No data" = 1, "No Data" = 1, "Data available" = 2)

# Map to use
#' Source: https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json
#' Modifications: Somaliland merged into Somalia, Northern Cyprus merged into Cyprus
geojson <- jsonlite::read_json(file.path("map", "countries.geo.json"))

# Store cache persistently
shinyOptions(cache = cachem::cache_disk(file.path("cache")))
