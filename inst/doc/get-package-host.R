## ----setup--------------------------------------------------------------------
library(DT)
library(risk.assessr)

## ----eval=FALSE---------------------------------------------------------------
# risk.assessr:::check_cran_package("here")

## ----eval=FALSE---------------------------------------------------------------
# html <- risk.assessr:::parse_package_info("here")
# html

## ----echo=FALSE, eval=FALSE---------------------------------------------------
# table <- risk.assessr:::parse_html_version(html, "here")
# pkg_df <- do.call(rbind, lapply(table, as.data.frame))
# datatable(pkg_df)

## ----eval=FALSE---------------------------------------------------------------
# version_info <- risk.assessr:::get_versions(table, "here")
# version_info$last_version

## ----echo=FALSE, eval=FALSE---------------------------------------------------
# pkg_df <- do.call(rbind, lapply(version_info$all_versions, as.data.frame))
# datatable(pkg_df)

## ----eval=FALSE---------------------------------------------------------------
# url <- risk.assessr:::get_cran_package_url(
#   package_name = "here",
#   version = NULL,
#   last_version = version_info$last_version,
#   all_versions = version_info$all_versions
# )
# url

## ----eval=FALSE---------------------------------------------------------------
# result_intern <- risk.assessr:::get_internal_package_url("herald")

## ----eval=FALSE---------------------------------------------------------------
# result_intern$url

## ----eval=FALSE---------------------------------------------------------------
# result_intern$last_version

## ----echo=FALSE, eval=FALSE---------------------------------------------------
# pkg_df <- do.call(rbind, lapply(result_intern$all_versions, as.data.frame))
# datatable(pkg_df)

## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------
# html_content <- fetch_bioconductor_releases()
# release_data <- parse_bioconductor_releases(html_content)
# result_bio <- get_bioconductor_package_url("flowCore", "2.18.0", release_data)

## ----eval=FALSE---------------------------------------------------------------
# result_bio$url

## ----eval=FALSE---------------------------------------------------------------
# urls <- c(
#   "https://github.com/tidyverse/ggplot2"
# )
# bug_reports <- c(
#   "https://github.com/tidyverse/ggplot2/issues"
# )
# 
# all_links <- c(urls, bug_reports)
# 
# github_pattern <- "https://github.com/([^/]+)/([^/]+).*"
# matching_links <- grep(github_pattern, all_links, value = TRUE)
# owner_names <- sub(github_pattern, "\\1", matching_links)
# package_names_github <- sub(github_pattern, "\\2", matching_links)
# 
# valid <- which(owner_names != "" & package_names_github != "")
# 
#   if (length(valid) > 0) {
#     github_links <- unique(paste0("https://github.com/", owner_names[valid], "/", package_names_github[valid]))
#   } else {
#     github_links <- NULL
#   }
# 
# github_links

