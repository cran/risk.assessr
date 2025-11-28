## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(risk.assessr)

## ----eval=FALSE---------------------------------------------------------------
# result <- risk.assessr::assess_pkg_r_package("ggplot2")
# generate_html_report(result)

## ----eval=FALSE---------------------------------------------------------------
# generate_html_report(result, output_dir= system.file("examples/", package = "risk.assessr"))

## ----echo=FALSE, eval=FALSE---------------------------------------------------
# file_path <- system.file("examples", "risk_report_ggplot2_3.5.2.html", package = "risk.assessr")
# 
# 
# if (interactive()) browseURL(file_path)

