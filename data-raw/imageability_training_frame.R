# data-raw/imageability_training_frame.R
#
# Build a merged word-level training frame for imageability modeling by
# combining aggregated Glasgow imageability targets with auxiliary lexical
# features derived from NoRaRe and simple word-level diagnostics.

normalize_word <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("\\s+", " ", x)
  x
}

load("data/glasgow_norms.rda")
load("data/imageability_model_features.rda")
load("data/glasgow_embeddings.rda")

glasgow_non_missing <- glasgow_norms[!is.na(glasgow_norms$imageability_m), ]
glasgow_non_missing$word <- normalize_word(glasgow_non_missing$word)

imageability_target <- aggregate(
  imageability_m ~ word,
  data = glasgow_non_missing[, c("word", "imageability_m")],
  FUN = mean
)

target_counts <- aggregate(
  imageability_m ~ word,
  data = glasgow_non_missing[, c("word", "imageability_m")],
  FUN = length
)
names(target_counts)[2] <- "glasgow_imageability_n"

context_counts <- aggregate(
  !is.na(context) ~ word,
  data = glasgow_non_missing[, c("word", "context")],
  FUN = sum
)
names(context_counts)[2] <- "glasgow_context_count"

variant_counts <- aggregate(
  word ~ word,
  data = glasgow_non_missing[, "word", drop = FALSE],
  FUN = length
)
variant_counts <- data.frame(
  word = names(table(glasgow_non_missing$word)),
  glasgow_variant_count = as.integer(table(glasgow_non_missing$word)),
  row.names = NULL,
  stringsAsFactors = FALSE
)

training_frame <- merge(imageability_target, target_counts, by = "word", all.x = TRUE, sort = FALSE)
training_frame <- merge(training_frame, context_counts, by = "word", all.x = TRUE, sort = FALSE)
training_frame <- merge(training_frame, variant_counts, by = "word", all.x = TRUE, sort = FALSE)
training_frame <- merge(training_frame, imageability_model_features, by = "word", all.x = TRUE, sort = FALSE)

training_frame$word_length <- nchar(training_frame$word)
training_frame$has_embedding <- training_frame$word %in% names(glasgow_embeddings)

feature_columns <- setdiff(
  names(training_frame),
  c(
    "word",
    "imageability_m",
    "glasgow_imageability_n",
    "glasgow_context_count",
    "glasgow_variant_count",
    "word_length",
    "has_embedding"
  )
)

for (feature_name in feature_columns) {
  missing_col <- paste0(feature_name, "_missing")
  training_frame[[missing_col]] <- as.integer(is.na(training_frame[[feature_name]]))
}

training_frame <- training_frame[order(training_frame$word), ]
row.names(training_frame) <- NULL

imageability_training_frame <- training_frame

coverage_summary <- data.frame(
  metric = c(
    "rows",
    "rows_with_embeddings",
    "rows_with_any_norare_feature",
    "rows_with_all_norare_features_missing",
    "mean_target_replicates",
    "max_target_replicates"
  ),
  value = c(
    nrow(imageability_training_frame),
    sum(imageability_training_frame$has_embedding),
    sum(rowSums(!is.na(imageability_training_frame[, feature_columns, drop = FALSE])) > 0),
    sum(rowSums(!is.na(imageability_training_frame[, feature_columns, drop = FALSE])) == 0),
    mean(imageability_training_frame$glasgow_imageability_n),
    max(imageability_training_frame$glasgow_imageability_n)
  ),
  stringsAsFactors = FALSE
)

dir.create("analysis", showWarnings = FALSE)
write.csv(coverage_summary, "analysis/imageability_training_frame_coverage.csv", row.names = FALSE)
save(
  imageability_training_frame,
  file = file.path("data", "imageability_training_frame.rda"),
  compress = "bzip2"
)

message(
  "Saved imageability_training_frame (", nrow(imageability_training_frame),
  " rows, ", ncol(imageability_training_frame), " columns)."
)
