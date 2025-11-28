## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----api-key-example, eval = FALSE--------------------------------------------
# # Replace with your actual API key
# options(pubmed.api_key = "your_actual_ncbi_api_key")
# 

## ----setup--------------------------------------------------------------------
library(risk.assessr)
library(ggplot2)

package_name <- "limma"

## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------
# pubmed_count <- get_pubmed_count(package_name)
# pubmed_count

## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------
# citation_data <- get_pubmed_by_year(package_name, years_back = 20)
# citation_data

## ----message=FALSE, warning=FALSE, echo=FALSE, eval=FALSE---------------------
# ggplot(citation_data, aes(x = Year, y = Count)) +
#   geom_col(fill = "steelblue") +
#   labs(
#     title = "PubMed Articles per Year",
#     subtitle = paste("Search term:", package_name),
#     x = "Publication Year",
#     y = "Number of Articles"
#   ) +
#   theme_minimal()

