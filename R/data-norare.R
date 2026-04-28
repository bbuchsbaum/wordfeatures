#' NoRaRe English Norms
#'
#' Long-format English lexical norms extracted from the NoRaRe CLDF release.
#' The table combines direct imageability ratings with related semantic features
#' that are useful for imageability modeling, including concreteness, sensory
#' experience, body-object interaction, affective ratings, familiarity, age of
#' acquisition, semantic size, and iconicity.
#'
#' Sources currently included:
#' Clark and Paivio (2004), Gilhooly and Logie (1980), Scott et al. (2019),
#' Brysbaert et al. (2014), Juhasz and Yap (2013), Warriner et al. (2013),
#' Pexman et al. (2019), and Winter et al. (2024).
#'
#' @name norare_english_norms
#' @docType data
#' @usage data(norare_english_norms)
#' @format A data frame with 12 columns:
#' \describe{
#'   \item{word}{Normalized lowercase word form used for joins.}
#'   \item{form}{Original surface form from NoRaRe.}
#'   \item{concept_id}{Concepticon-linked concept identifier from NoRaRe.}
#'   \item{dataset_id}{NoRaRe dataset identifier.}
#'   \item{dataset_name}{Human-readable dataset name.}
#'   \item{year}{Publication year for the source dataset.}
#'   \item{source_url}{Source URL recorded by NoRaRe.}
#'   \item{citation}{Citation key or label from NoRaRe.}
#'   \item{variable_id}{NoRaRe variable identifier.}
#'   \item{feature_name}{Package-friendly feature name.}
#'   \item{measurement}{Semantic dimension being measured.}
#'   \item{value}{Numeric norm value.}
#' }
#' @source \url{https://norare.clld.org}
NULL

#' Imageability Model Features
#'
#' Wide-format lexical feature table derived from `norare_english_norms`. Each
#' row is a normalized English word and each column is a candidate predictor or
#' label source that may help train imageability models.
#'
#' @name imageability_model_features
#' @docType data
#' @usage data(imageability_model_features)
#' @format A data frame with one row per word and feature columns such as
#' `imageability_clark_2004`, `imageability_gilhooly_logie_1980`,
#' `imageability_glasgow_2019`, `concreteness_brysbaert_2014`,
#' `sensory_experience_juhasz_2013`, `boi_pexman_2019`,
#' `valence_warriner_2013`, and `iconicity_winter_2024`.
#' @source \url{https://norare.clld.org}
NULL
