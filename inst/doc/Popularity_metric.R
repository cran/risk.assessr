## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

knitr::opts_chunk$set(fig.width = 12, fig.height = 5, out.width = "100%" ) 

## ----setup, include = FALSE---------------------------------------------------
library(risk.assessr)
library(ggplot2)
library(dplyr)

suppressMessages({
  library(kableExtra)
  library(knitr)
})

suppressWarnings({
  library(htmltools)
  library(htmlwidgets)
})

## ----cran-data, eval=FALSE----------------------------------------------------
# download_cran_df <- get_package_download_cran("admiral", years = 5)
# head(download_cran_df)

## ----echo=FALSE, cran-data-viz, eval=FALSE------------------------------------
# 
# monthly_downloads <- download_cran_df %>%
#   mutate(month = format(date, "%Y-%m")) %>%
#   group_by(month) %>%
#   summarise(
#     downloads = sum(count),
#     .groups = "drop"
#   ) %>%
#   mutate(month = as.Date(paste0(month, "-01")))
# 
# head(monthly_downloads)

## ----eval=FALSE---------------------------------------------------------------
# version_infos <- check_and_fetch_cran_package("admiral")

## ----echo=FALSE, eval=FALSE---------------------------------------------------
# version_dates <- bind_rows(lapply(version_infos$all_versions, as.data.frame)) %>%
#   mutate(date = as.Date(date))
# 
# ggplot(monthly_downloads, aes(x = month, y = downloads)) +
#   geom_line(color = "#2C3E50") +
#   geom_point(color = "#E74C3C", size = 1) +
#   geom_vline(
#     data = version_dates,
#     aes(xintercept = as.numeric(date)),
#     color = "blue",
#     linetype = "dashed",
#     linewidth = 0.5
#   ) +
#   labs(
#     title = "Monthly Downloads of 'admiral' Package",
#     x = "Month",
#     y = "Number of Downloads"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 10, t = 20)),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     plot.margin = margin(t = 30, r = 20, b = 20, l = 20)
#   )

## ----github-meta, eval=FALSE--------------------------------------------------
# repo_info <- get_github_data("tidyverse", "ggplot2")
# repo_info

## ----github-data, eval=FALSE--------------------------------------------------
# df_commit <- get_commits_since("tidyverse", "ggplot2", years = 1)
# head(df_commit)

## ----echo=FALSE, github-data-viz, eval=FALSE----------------------------------
# df_commit <- df_commit %>%
#   arrange(week_start) %>%
#   mutate(cumulative_commits = cumsum(n_commits))
# 
# ggplot(df_commit, aes(x = week_start)) +
#   geom_col(aes(y = n_commits), fill = "#1ABC9C") +
#   geom_line(aes(y = cumulative_commits), color = "#34495E", linewidth = 1, linetype = "dashed") +
#   labs(
#     title = "Weekly Commits on GitHub (ggplot2)",
#     x = "Week",
#     y = "Number of Commits"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     plot.title = element_text(face = "bold", hjust = 0.5),
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )

## ----eval=FALSE---------------------------------------------------------------
# download_bio_df <- get_package_download_bioconductor("limma")
# head(download_bio_df$all_data)

## ----echo=FALSE, message=FALSE, eval=FALSE------------------------------------
# html_content <- fetch_bioconductor_releases()
# release_data <- parse_bioconductor_releases(html_content)
# info <- get_bioconductor_package_url("limma", release_data = release_data)

## ----bioconductor-data, eval=FALSE--------------------------------------------
# download_bio_df <- get_package_download_bioconductor("DESeq2")
# head(download_bio_df$all_data)

## ----echo=FALSE, bioconductor-data-viz, eval=FALSE----------------------------
# ggplot(download_bio_df$all_data, aes(x = Date, y = Nb_of_downloads)) +
#   geom_line(color = "#9B59B6") +
#   geom_point(color = "#2980B9") +
#   labs(
#     title = "Bioconductor Downloads",
#     x = "Month",
#     y = "Number of Downloads"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     plot.title = element_text(
#       face = "bold",
#       hjust = 0.5,
#       margin = margin(b = 10, t = 20)
#     ),
#     plot.margin = margin(t = 30, r = 20, b = 20, l = 20),
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )

