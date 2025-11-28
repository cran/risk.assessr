## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(risk.assessr)
options(repos = "http://cran.us.r-project.org")

## ----run_tm_no_test, message=FALSE, warning=FALSE, eval=FALSE-----------------
# stringr_tm_no_test <- risk.assessr::generate_traceability_matrix("stringr", version = "1.5.1" )

## ----tm_list_no_test, eval=FALSE----------------------------------------------
# str(stringr_tm_no_test$tm)

## ----run_tm_test, message=FALSE, warning=FALSE, eval=FALSE--------------------
# stringr_tm_test <- risk.assessr::generate_traceability_matrix("stringr", version = "1.5.1", execute_coverage = TRUE)

## ----tm_list_test, eval=FALSE-------------------------------------------------
# str(stringr_tm_test$tm)

## ----run_full_analysis, message=FALSE, warning=FALSE, eval=FALSE--------------
# stringr <- risk.assessr::assess_pkg_r_package("stringr", version = "1.5.1" )

## ----tm_list, eval=FALSE------------------------------------------------------
# str(stringr$tm_list)

## ----tm_list-tm, eval=FALSE---------------------------------------------------
# stringr$tm_list$tm

## ----tm_list-coverage, eval=FALSE---------------------------------------------
# stringr$tm_list$coverage

## ----high-risk, eval=FALSE----------------------------------------------------
# stringr$tm_list$coverage$high_risk

## ----medium-risk, eval=FALSE--------------------------------------------------
# stringr$tm_list$coverage$medium_risk

## ----low-risk, eval=FALSE-----------------------------------------------------
# low_risk <- stringr$tm_list$coverage$low_risk
# low_risk

## ----defunct-functions, eval=FALSE--------------------------------------------
# stringr$tm_list$function_type

## ----imported-summary, eval=FALSE---------------------------------------------
# stringr$tm_list$function_type$imported

## ----rexported-summary, eval=FALSE--------------------------------------------
# stringr$tm_list$function_type$rexported

## ----experimental-summary, eval=FALSE-----------------------------------------
# stringr$tm_list$function_type$experimental

