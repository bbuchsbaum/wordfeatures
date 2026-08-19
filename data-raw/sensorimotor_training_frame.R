# data-raw/sensorimotor_training_frame.R
#
# Build a word-level modeling frame from the Lancaster Sensorimotor Norms.
# Embeddings are not stored here; has_embedding records coverage in
# glasgow_embeddings and/or the local sensorimotor embedding cache.

normalize_word <- function(x) {
  x <- tolower(trimws(x))
  gsub("\\s+", " ", x)
}

lancaster_cols <- c(
  visual = "Visual.mean",
  auditory = "Auditory.mean",
  haptic = "Haptic.mean",
  olfactory = "Olfactory.mean",
  gustatory = "Gustatory.mean",
  interoceptive = "Interoceptive.mean",
  hand_arm = "Hand_arm.mean",
  foot_leg = "Foot_leg.mean",
  head = "Head.mean",
  mouth = "Mouth.mean",
  torso = "Torso.mean"
)

sensorimotor_family <- function(words) {
  vapply(words, function(word) {
    tokens <- unlist(strsplit(word, "\\s+", perl = TRUE), use.names = FALSE)
    tokens <- tokens[nzchar(tokens)]
    if (length(tokens) == 0) {
      return("")
    }
    stems <- SnowballC::wordStem(tokens, language = "en")
    paste(sort(stems), collapse = "_")
  }, character(1), USE.NAMES = FALSE)
}

load_cached_embedding_words <- function() {
  combined_path <- file.path("data-raw", "cache", "sensorimotor_embeddings", "combined.rds")
  if (file.exists(combined_path)) {
    cached <- readRDS(combined_path)
    return(as.character(cached$words))
  }

  batch_dir <- file.path("data-raw", "cache", "sensorimotor_embeddings")
  if (!dir.exists(batch_dir)) {
    return(character())
  }

  batch_files <- list.files(batch_dir, pattern = "^batch_.*\\.rds$", full.names = TRUE)
  if (length(batch_files) == 0) {
    return(character())
  }

  unique(unlist(lapply(batch_files, function(path) {
    names(readRDS(path))
  }), use.names = FALSE))
}

if (!requireNamespace("SnowballC", quietly = TRUE)) {
  stop("SnowballC is required to assign morphological families.")
}

load("data/sensorimotor_norms.rda")

glasgow_words <- character()
if (file.exists("data/glasgow_embeddings.rda")) {
  load("data/glasgow_embeddings.rda")
  glasgow_words <- names(glasgow_embeddings)
}

training_frame <- data.frame(
  word = normalize_word(sensorimotor_norms$Word),
  stringsAsFactors = FALSE
)

for (out_name in names(lancaster_cols)) {
  training_frame[[out_name]] <- as.numeric(sensorimotor_norms[[lancaster_cols[[out_name]]]])
}

if (anyDuplicated(training_frame$word)) {
  stop("Normalized Lancaster words are not unique; inspect sensorimotor_norms$Word.")
}

training_frame$word_length <- nchar(training_frame$word)
training_frame$family <- sensorimotor_family(training_frame$word)

cached_words <- load_cached_embedding_words()
training_frame$has_embedding <- training_frame$word %in% unique(c(glasgow_words, cached_words))

training_frame <- training_frame[order(training_frame$word), ]
row.names(training_frame) <- NULL

sensorimotor_training_frame <- training_frame

coverage_summary <- data.frame(
  metric = c(
    "rows",
    "unique_families",
    "rows_with_embeddings",
    "multiword_rows",
    "mean_visual",
    "mean_olfactory",
    "mean_gustatory"
  ),
  value = c(
    nrow(sensorimotor_training_frame),
    length(unique(sensorimotor_training_frame$family)),
    sum(sensorimotor_training_frame$has_embedding),
    sum(grepl(" ", sensorimotor_training_frame$word)),
    mean(sensorimotor_training_frame$visual),
    mean(sensorimotor_training_frame$olfactory),
    mean(sensorimotor_training_frame$gustatory)
  ),
  stringsAsFactors = FALSE
)

dir.create("analysis", showWarnings = FALSE)
write.csv(
  coverage_summary,
  "analysis/sensorimotor_training_frame_coverage.csv",
  row.names = FALSE
)

save(
  sensorimotor_training_frame,
  file = file.path("data", "sensorimotor_training_frame.rda"),
  compress = "bzip2"
)

message(
  "Saved sensorimotor_training_frame (",
  nrow(sensorimotor_training_frame), " rows, ",
  ncol(sensorimotor_training_frame), " columns; ",
  sum(sensorimotor_training_frame$has_embedding), " with embeddings)."
)
