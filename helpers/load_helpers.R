read_world_bank_classification <- function() {
  wb_classification <- readxl::read_xls("data/WorldBank Classification.xls", skip = 4)
  setDT(wb_classification)
  wb_classification <- wb_classification[!is.na(`Income group`) & `Income group` != "x", .(Code, `Income group`)]
  wb_classification[, Income := `Income group`]
  wb_classification[, `Income group` := NULL]
  wb_classification
}

read_tx_policy <- function(file_path) {
  tx_policy <- readxl::read_xlsx(file_path, skip = 4)
  setDT(tx_policy)
  names(tx_policy) <- c(
    "Country",
    "ISO",
    "World Bank Classification",
    "Region",
    "Covered in MPP voluntary licence territory for Molnupiravir generics?",
    "Covered in MPP voluntary licence territory for Nirmatrelvir/Ritonavir generics?",
    "Included in ACT-A partner access agreement for Molnupiravir?",
    "Included in ACT-A partner access agreement for Nirmatrelvir/Ritonavir?"
  )
  
  tx_policy$`World Bank Classification` <- NULL
  tx_policy$`Region` <- NULL
  tx_policy$`Country` <- NULL
  
  tx_policy
}

read_dx_policy <- function(file_path) {
  dx_policy <- readxl::read_xlsx(file_path)
  setDT(dx_policy)
  
  # Filter out NA Country
  dx_policy <- dx_policy[!is.na(Country), ]
  
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
  
  dx_policy[, `Policy links` := str_replace_all(`Policy links`, pattern = "(\\r)*\\n", replacement = "<br>")]
  dx_policy[, `Policy links` := str_replace_all(`Policy links`, pattern = "(http(s)?://[^<]+)", replacement = "<a href='\\1'>\\1</a>")]
  
  # Merge continent -------------------------------
  codelist <- setNames(countrycode::codelist[, c("iso2c", "iso3c", "continent", "iso.name.en")], c("iso2c", "iso3c", "Continent", "iso.name.en"))
  codelist <- codelist[!is.na(codelist$iso.name.en), ]
  
  codelist[codelist$iso.name.en %in% "Tanzania, the United Republic of", "iso.name.en"] <- "Tanzania"
  dx_policy[Country %in% "Tanzania", "ISO"] <- "TZA"
  
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
  
  # Set column order
  setcolorder(dx_policy, c("Flag", "Country", "Continent", "Income", "Date of last update"))
  
  # Return dataset
  dx_policy
}
