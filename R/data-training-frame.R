#' Imageability Training Frame
#'
#' Word-level training frame for imageability modeling. The target is mean
#' Glasgow imageability per normalized word, aggregated across duplicated
#' contextual variants where present. The predictors combine auxiliary lexical
#' features derived from `imageability_model_features` with simple join and
#' coverage diagnostics such as word length, the number of Glasgow variants, and
#' missingness indicators for each imported NoRaRe feature.
#'
#' @name imageability_training_frame
#' @docType data
#' @usage data(imageability_training_frame)
#' @format A data frame with one row per normalized word. Columns:
#' \describe{
#'   \item{word}{Normalized lowercase join key.}
#'   \item{imageability_m}{Mean Glasgow imageability target for the word.}
#'   \item{glasgow_imageability_n}{Number of Glasgow rows contributing to the target.}
#'   \item{glasgow_context_count}{Number of contributing rows with a non-missing context label.}
#'   \item{glasgow_variant_count}{Number of contributing Glasgow variants for the word.}
#'   \item{aoa_gilhooly_logie_1980}{Age-of-acquisition rating from Gilhooly and Logie (1980).}
#'   \item{aoa_glasgow_2019}{Age-of-acquisition rating from the Glasgow norms (2019).}
#'   \item{arousal_glasgow_2019}{Arousal rating from the Glasgow norms (2019).}
#'   \item{arousal_warriner_2013}{Arousal rating from Warriner et al. (2013).}
#'   \item{boi_pexman_2019}{Body-object interaction rating from Pexman et al. (2019).}
#'   \item{concreteness_brysbaert_2014}{Concreteness rating from Brysbaert et al. (2014).}
#'   \item{concreteness_gilhooly_logie_1980}{Concreteness rating from Gilhooly and Logie (1980).}
#'   \item{concreteness_glasgow_2019}{Concreteness rating from the Glasgow norms (2019).}
#'   \item{dominance_glasgow_2019}{Dominance rating from the Glasgow norms (2019).}
#'   \item{dominance_warriner_2013}{Dominance rating from Warriner et al. (2013).}
#'   \item{familiarity_clark_2004}{Familiarity rating from Clark and Paivio (2004).}
#'   \item{familiarity_gilhooly_logie_1980}{Familiarity rating from Gilhooly and Logie (1980).}
#'   \item{familiarity_glasgow_2019}{Familiarity rating from the Glasgow norms (2019).}
#'   \item{iconicity_winter_2024}{Iconicity rating from Winter et al. (2024).}
#'   \item{imageability_clark_2004}{Imageability rating from Clark and Paivio (2004).}
#'   \item{imageability_gilhooly_logie_1980}{Imageability rating from Gilhooly and Logie (1980).}
#'   \item{imageability_glasgow_2019}{Imageability rating from the Glasgow norms (2019).}
#'   \item{semantic_size_glasgow_2019}{Semantic size rating from the Glasgow norms (2019).}
#'   \item{sensory_experience_juhasz_2013}{Sensory experience rating from Juhasz and Yap (2013).}
#'   \item{valence_glasgow_2019}{Valence rating from the Glasgow norms (2019).}
#'   \item{valence_warriner_2013}{Valence rating from Warriner et al. (2013).}
#'   \item{word_length}{Number of characters in the normalized word.}
#'   \item{has_embedding}{Whether `glasgow_embeddings` contains a vector for the word.}
#'   \item{aoa_gilhooly_logie_1980_missing}{Missingness indicator for `aoa_gilhooly_logie_1980`.}
#'   \item{aoa_glasgow_2019_missing}{Missingness indicator for `aoa_glasgow_2019`.}
#'   \item{arousal_glasgow_2019_missing}{Missingness indicator for `arousal_glasgow_2019`.}
#'   \item{arousal_warriner_2013_missing}{Missingness indicator for `arousal_warriner_2013`.}
#'   \item{boi_pexman_2019_missing}{Missingness indicator for `boi_pexman_2019`.}
#'   \item{concreteness_brysbaert_2014_missing}{Missingness indicator for `concreteness_brysbaert_2014`.}
#'   \item{concreteness_gilhooly_logie_1980_missing}{Missingness indicator for `concreteness_gilhooly_logie_1980`.}
#'   \item{concreteness_glasgow_2019_missing}{Missingness indicator for `concreteness_glasgow_2019`.}
#'   \item{dominance_glasgow_2019_missing}{Missingness indicator for `dominance_glasgow_2019`.}
#'   \item{dominance_warriner_2013_missing}{Missingness indicator for `dominance_warriner_2013`.}
#'   \item{familiarity_clark_2004_missing}{Missingness indicator for `familiarity_clark_2004`.}
#'   \item{familiarity_gilhooly_logie_1980_missing}{Missingness indicator for `familiarity_gilhooly_logie_1980`.}
#'   \item{familiarity_glasgow_2019_missing}{Missingness indicator for `familiarity_glasgow_2019`.}
#'   \item{iconicity_winter_2024_missing}{Missingness indicator for `iconicity_winter_2024`.}
#'   \item{imageability_clark_2004_missing}{Missingness indicator for `imageability_clark_2004`.}
#'   \item{imageability_gilhooly_logie_1980_missing}{Missingness indicator for `imageability_gilhooly_logie_1980`.}
#'   \item{imageability_glasgow_2019_missing}{Missingness indicator for `imageability_glasgow_2019`.}
#'   \item{semantic_size_glasgow_2019_missing}{Missingness indicator for `semantic_size_glasgow_2019`.}
#'   \item{sensory_experience_juhasz_2013_missing}{Missingness indicator for `sensory_experience_juhasz_2013`.}
#'   \item{valence_glasgow_2019_missing}{Missingness indicator for `valence_glasgow_2019`.}
#'   \item{valence_warriner_2013_missing}{Missingness indicator for `valence_warriner_2013`.}
#' }
#' @source Built from `glasgow_norms`, `glasgow_embeddings`, and
#' `imageability_model_features`.
NULL
