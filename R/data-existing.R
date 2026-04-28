#' Glasgow Norms
#'
#' Processed Glasgow Norms data with psycholinguistic ratings for English words,
#' including imageability, concreteness, familiarity, age of acquisition,
#' affective ratings, semantic size, and gender association.
#'
#' @name glasgow_norms
#' @docType data
#' @usage data(glasgow_norms)
#' @format A data frame with one row per word or word sense and columns such as
#' `word`, `context`, `imageability_m`, `concreteness_m`, `familiarity_m`,
#' `aoa_m`, `valence_m`, `arousal_m`, and `dominance_m`.
#' @source Scott et al. (2019), Glasgow Norms.
NULL

#' Sensorimotor Norms
#'
#' Processed English sensorimotor norms with ratings across perceptual
#' modalities and action effectors.
#'
#' @name sensorimotor_norms
#' @docType data
#' @usage data(sensorimotor_norms)
#' @format A data frame with one row per word and columns such as `Word`,
#' `Visual.mean`, `Auditory.mean`, `Haptic.mean`, `Interoceptive.mean`,
#' `Gustatory.mean`, `Olfactory.mean`, and action strength ratings for multiple
#' body effectors.
#' @source Lynott et al. (2020), Lancaster Sensorimotor Norms.
NULL

#' Glasgow Embeddings
#'
#' A named list of embedding vectors for words in `glasgow_norms`.
#'
#' @name glasgow_embeddings
#' @docType data
#' @usage data(glasgow_embeddings)
#' @format A named list where each element is a numeric embedding vector and the
#' list names are normalized word forms from the Glasgow norms.
#' @source Generated from package embedding utilities for words in the Glasgow
#' norms.
NULL
