## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(risk.assessr)

## ----check and fetch, eval=FALSE----------------------------------------------
# result_cran <- check_and_fetch_cran_package("admiral", "1.0.0")

## ----url, eval=FALSE----------------------------------------------------------
# result_cran$package_url

## ----last_version, eval=FALSE-------------------------------------------------
# result_cran$last_version

## ----all_versions, eval=FALSE-------------------------------------------------
# head(result_cran$all_versions, n = 2)

## ----version_history, echo=FALSE, eval=FALSE----------------------------------
# # Example: flatten result$all_versions
# version_history <- do.call(rbind, lapply(result_cran$all_versions, function(entry) {
#   data.frame(
#     Version = entry$version,
#     Date = as.Date(entry$date),
#     stringsAsFactors = FALSE
#   )
# }))
# 
# # Show nicely
# knitr::kable(version_history, caption = "CRAN Version History")

## ----bio_fetch, eval=FALSE----------------------------------------------------
# html_content <- fetch_bioconductor_releases()
# release_data <- parse_bioconductor_releases(html_content)

## ----bio_release, echo=FALSE, eval=FALSE--------------------------------------
# # Example: flatten result$all_versions
# release <- do.call(rbind, lapply(release_data, function(entry) {
#   data.frame(
#     Version = entry$release,
#     Date = entry$date,
#     software_packages = entry$software_packages,
#     r_version = entry$r_version,
#     stringsAsFactors = FALSE
#   )
# }))
# 
# # Show nicely
# knitr::kable(release, caption = "release Bioconductor versions")

## ----flowCore, eval=FALSE-----------------------------------------------------
# fetch_bioconductor_package_info("3.21", "flowCore")

## ----result_flowCore, eval=FALSE----------------------------------------------
# html_content <- fetch_bioconductor_releases()
# release_data <- parse_bioconductor_releases(html_content)
# result_bio <- get_bioconductor_package_url("flowCore", "2.18.0", release_data)

## ----bio_all_versions, eval=FALSE---------------------------------------------
# head(result_bio$all_versions, n=2)

## ----bio_last_version, eval=FALSE---------------------------------------------
# result_bio$last_version

## ----bio_package_versions, eval=FALSE-----------------------------------------
# result_bio$bioconductor_version_package

## ----bio_url, eval=FALSE------------------------------------------------------
# result_bio$url

## ----bio_archived, eval=FALSE-------------------------------------------------
# result_bio$archived

