## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(risk.assessr)
options(repos = c(CRAN = "http://cran.us.r-project.org"))

## ----message=FALSE, warning=FALSE---------------------------------------------
result_default <- risk.assessr::assess_pkg_r_package("stringr")

## ----message=FALSE------------------------------------------------------------
str(result_default$risk_analysis)

## ----message=FALSE, warning=FALSE---------------------------------------------

strict_coverage_config <- list(
  list(
    label = "code coverage",
    id = "code_coverage",
    key = "code_coverage",
    thresholds = list(
      list(level = "high", max = 0.9999),
      list(level = "low", max = NULL)
    )
  ),
  list(
    label = "popularity",
    id = "popularity",
    key = "last_month_download",
    thresholds = list(
      list(level = "high", max = 21200000),          
      list(level = "medium", max = 11200000),      
      list(level = "low", max = NULL)       
    )
  )
)

# Set the option
options(risk.assessr.risk_definition = strict_coverage_config)
result_strict <- risk.assessr::assess_pkg_r_package("stringr")


## -----------------------------------------------------------------------------
str(result_strict$risk_analysis)

