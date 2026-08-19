# Shared word-key normalization used by imageability and sensorimotor joins.
.normalize_words <- function(words) {
  words <- tolower(trimws(words))
  gsub("\\s+", " ", words)
}
