## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(risk.assessr)

## ----acquisition, eval=FALSE--------------------------------------------------
# here <- risk.assessr::assess_pkg_r_package("here")

## ----eval=FALSE---------------------------------------------------------------
# here$results[c("pkg_name", "pkg_version", "sysname", "release", "date_time")]

## ----eval=FALSE---------------------------------------------------------------
# here$results[c("license_name", "has_examples", "has_vignettes", "has_news",
#                "has_website", "has_source_control", "export_help",
#                "export_calc", "covr", "check")]

## ----eval=FALSE---------------------------------------------------------------
# here$results$dependencies

## ----eval=FALSE---------------------------------------------------------------
# here$covr_list$res_cov$coverage$totalcoverage

## ----eval=FALSE---------------------------------------------------------------
# here$check_list$res_check$errors

## ----eval=FALSE---------------------------------------------------------------
# here$results$download

## ----eval=FALSE---------------------------------------------------------------
# here$results$github_data

## ----eval=FALSE---------------------------------------------------------------
# head(here$results$rev_deps, 10)

## ----eval=FALSE---------------------------------------------------------------
# here$results$author

## ----eval=FALSE---------------------------------------------------------------
# here$results$host

## ----eval=FALSE---------------------------------------------------------------
# here$risk_analysis

## ----eval=FALSE---------------------------------------------------------------
# here$tm_list

## ----eval=FALSE---------------------------------------------------------------
# here$results$suggested_deps

