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

material_dep <- htmltools::htmlDependency(
  name = "material-ui",
  version = "4.6.1",
  src = c(href = "https://unpkg.com/@material-ui/core/umd/"),
  script = "material-ui.production.min.js"
)

# Read helper functions
helper_files <- dir("helpers", full.names = TRUE)
lapply(helper_files, source)

# World Bank classification ---------------------
wb_classification <- readxl::read_xls("data/WorldBank Classification.xls", skip = 4)
setDT(wb_classification)
wb_classification <- wb_classification[!is.na(`Income group`) & `Income group` != "x", .(Code, `Income group`)]
wb_classification[, Income := `Income group`]
wb_classification[, `Income group` := NULL]

# Read dx policy --------------------------------
dx_policy <- readxl::read_xlsx("data/March 29 Deliverable_Updated Policy Mapping Template.xlsx")
setDT(dx_policy)
dx_policy[, `Date of last update` := as.character(`Date of last update`)]
dx_policy[, `Notes` := NULL]

# Simplify questions
setnames(dx_policy, 
         old = c(
           "Does the country have a policy that guides Covid-19 testing strategy?",
           "Is molecular testing registered for use in country?",
           "Is molecular testing used to confirm a Covid-19 diagnosis?",
           "Are antigen rapid tests registered for use in country?",
           "Are antigen rapid tests used to confirm Covid-19 diagnosis?",
           "Are antigen rapid tests used for the testing of symptomatic patients?",
           "Are antigen rapid tests used for the screening of asymptomatic patients?",
           
           "Are antigen rapid tests used for asymptomatic contacts of known positives (i.e., contact tracing)?",
           "Are antigen rapid tests used for testing of health care workers / front line staff?",
           "Are antigen rapid tests used for testing at borders / points of entry?",
           "Are antigen rapid tests used for testing at schools / workplaces?",
           "Are antigen rapid tests used for testing for non covid-19 hospitalized patients (e.g., scheduled or elective surgery)?",
           "Who is allowed to use the Ag-RDTs (only health workers etc)?",

           "Are antibody rapid tests registered for use in country?",
           "Are antibody rapid tests used to confirm a Covid-19 diagnosis?",
           "Are antibody rapid tests used for serosurveillance studies of Covid-19?"
         ), 
         new = c(
           "Covid-19 testing strategy available",
           "Molecular test registered in country",
           "Molecular test used to confirm covid-19 diagnosis",
           "Antigen rapid tests registered in country",
           "Antigen rapid tests used to confirm covid-19 diagnosis",
           "Antigen rapid tests used for testing symptomatic patients",
           "Antigen rapid tests used for testing asymptomatic patients",
           
           "Antigen rapid tests used for contact tracing",
           "Antigen rapid tests used for health care workers",
           "Antigen rapid tests used at borders",
           "Antigen rapid tests used at schools/workplaces",
           "Antigen rapid tests used for non covid-19 hospitalized patients",
           "Any limitations on who can use antigen rapid tests",
           
           "Antibody rapid tests registered in country",
           "Antibody rapid tests used to confirm covid-19 diagnosis",
           "Antibody rapid tests used for serosurveillance studies of covid-19"
         ))




## Gather Policy Links into one field
# policy_links_cols <- str_subset(names(dx_policy), pattern = "^Policy Links")
# dx_policy[, `Policy Links` := do.call(paste, c(.SD, sep = "<br>")), .SDcols = policy_links_cols]

# Remove extra spaces from colnames
names(dx_policy) <- str_replace_all(names(dx_policy), pattern = " +", replacement = " ")

# Turn policy links into links ------------------
## Use <br> for linebreaks
# dx_policy[, `Policy Links` := str_remove_all(`Policy Links`, pattern = "<br>NA")]
dx_policy[, `Policy Links` := str_remove_all(`Policy Links`, pattern = "\\n")]

## Use <a> for links
# dx_policy[, `Policy Links` := str_replace_all(`Policy Links`, pattern = "(http(s)?://[^<]+)", replacement = "<a href='\\1'>\\1</a>")]

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
                  "Covid-19 testing strategy available",
                  "Molecular test registered in country",
                  "Molecular test used to confirm covid-19 diagnosis",
                  "Antibody rapid tests registered in country",
                  "Antibody rapid tests used to confirm covid-19 diagnosis",
                  "Antibody rapid tests used for serosurveillance studies of covid-19",
                  "Antigen rapid tests registered in country",
                  "Antigen rapid tests used to confirm covid-19 diagnosis",
                  "Antigen rapid tests used for testing symptomatic patients")

testing_cols <- c("Covid-19 testing strategy available",
  
                  "Molecular test registered in country",
                  "Molecular test used to confirm covid-19 diagnosis",

                  "Antigen rapid tests registered in country",
                  "Antigen rapid tests used to confirm covid-19 diagnosis",
                  "Antigen rapid tests used for testing symptomatic patients",
                  "Antigen rapid tests used for testing asymptomatic patients",
                  "Antigen rapid tests used for contact tracing",
                  "Antigen rapid tests used for health care workers",
                  "Antigen rapid tests used at borders",
                  "Antigen rapid tests used at schools/workplaces",
                  "Antigen rapid tests used for non covid-19 hospitalized patients",
                  
                  "Antibody rapid tests registered in country",
                  "Antibody rapid tests used to confirm covid-19 diagnosis",
                  "Antibody rapid tests used for serosurveillance studies of covid-19"
)

column_choices <- list(
  General = c(
    "Country",
    "Continent",
    "Income",
    "Date of last update",
    "Covid-19 testing strategy available",
    "Policy Links"
  ),
  `Molecular testing` = c(
    "Molecular test registered in country",
    "Molecular test used to confirm covid-19 diagnosis"
  ),
  `Antibody testing` = c(
    "Antibody rapid tests registered in country",                                                        
    "Antibody rapid tests used to confirm covid-19 diagnosis",
    "Antibody rapid tests used for serosurveillance studies of covid-19"
  ),
  `Antigen testing` = c(
    "Antigen rapid tests registered in country",
    "Antigen rapid tests used to confirm covid-19 diagnosis",
    "Antigen rapid tests used for testing symptomatic patients",
    "Antigen rapid tests used for testing asymptomatic patients",
    "Antigen rapid tests used for contact tracing",
    "Antigen rapid tests used for health care workers",
    "Antigen rapid tests used at borders",
    "Antigen rapid tests used at schools/workplaces",
    "Antigen rapid tests used for non covid-19 hospitalized patients",
    "Any limitations on who can use antigen rapid tests"
  )
)

dx_policy[, (testing_cols) := lapply(.SD, function(x) {
  ifelse(x == "No Data", "No data", x)
}), .SDcols = testing_cols]

dx_policy[, (testing_cols) := lapply(.SD, function(x) {
  ifelse(x == "yes", "Yes", x)
}), .SDcols = testing_cols]

setcolorder(dx_policy, c("Flag", "Country", "Continent", "Income", "Date of last update", 
                         "Covid-19 testing strategy available",
                         column_choices$`Molecular testing`,
                         column_choices$`Antigen testing`,
                         column_choices$`Antibody testing`
))

# Convert columns to factor/date ----------------
factor_cols <- public_cols[!public_cols %in% c("Policy Links", "Date of last update")]
dx_policy[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]
dx_policy[, `Date of last update` := as_date(`Date of last update`)]

value_lookup <- c("NA" = 1, "No data" = 1, "No" = 2, "Yes" = 3)
