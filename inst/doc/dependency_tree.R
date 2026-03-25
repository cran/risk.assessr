## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(risk.assessr)


tryCatch({
  options(repos = c(CRAN = "http://cran.us.r-project.org"))
}, error = function(e) {
  message("No CRAN available")
})

## -----------------------------------------------------------------------------
download_and_parse_dependencies("stringr")

## -----------------------------------------------------------------------------
dep_data <- fetch_all_dependencies("stringr")

## -----------------------------------------------------------------------------
dep_data

## -----------------------------------------------------------------------------
print_tree(dep_data)

## -----------------------------------------------------------------------------
dep_data_with_license <- fetch_all_dependencies("stringr", get_license = TRUE)

## -----------------------------------------------------------------------------
print_tree(dep_data_with_license)

## -----------------------------------------------------------------------------
# Shallow exploration (only 2 levels)
dep_data_shallow <- fetch_all_dependencies("stringr", get_license = TRUE, max_level = 2)
print_tree(dep_data_shallow)

## -----------------------------------------------------------------------------
# Deeper exploration (5 levels)
dep_data_deep <- fetch_all_dependencies("stringr", get_license = TRUE, max_level = 5)
print_tree(dep_data_deep)

## -----------------------------------------------------------------------------
detect_version_conflicts(dep_data)

