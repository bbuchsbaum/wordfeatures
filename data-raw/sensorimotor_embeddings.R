#!/usr/bin/env Rscript
#
# Fetch OpenAI text-embedding-3-large vectors for every Lancaster word.
# Reuses glasgow_embeddings where the normalized word already exists.
# Writes per-batch RDS files so the job can be resumed. The combined
# matrix is cached locally and is NOT packaged.

provider <- "openai"
openai_model <- "text-embedding-3-large"
batch_size <- 100
pause_seconds <- 1
cache_dir <- file.path("data-raw", "cache", "sensorimotor_embeddings")
combined_path <- file.path(cache_dir, "combined.rds")

normalize_word <- function(x) {
  x <- tolower(trimws(x))
  gsub("\\s+", " ", x)
}

if (!exists("word_embeddings")) {
  if (requireNamespace("devtools", quietly = TRUE)) {
    message("Loading package 'wordfeatures' using devtools::load_all()")
    devtools::load_all()
  } else {
    stop("Load wordfeatures first (devtools::load_all() or library(wordfeatures)).")
  }
}

if (provider == "openai" && !nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  stop("OPENAI_API_KEY environment variable is not set.")
}

if (!file.exists("data/sensorimotor_training_frame.rda")) {
  stop("Run data-raw/sensorimotor_training_frame.R first.")
}

load("data/sensorimotor_training_frame.rda")
target_words <- unique(sensorimotor_training_frame$word)
target_words <- target_words[!is.na(target_words) & nzchar(target_words)]

dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

have_words <- character()

if (file.exists("data/glasgow_embeddings.rda")) {
  load("data/glasgow_embeddings.rda")
  overlap <- intersect(target_words, names(glasgow_embeddings))
  if (length(overlap) > 0) {
    reuse_path <- file.path(cache_dir, "glasgow_reuse.rds")
    reuse <- glasgow_embeddings[overlap]
    names(reuse) <- overlap
    saveRDS(reuse, reuse_path)
    have_words <- c(have_words, overlap)
    message("Reused ", length(overlap), " Glasgow embeddings.")
  }
}

existing_batch_files <- list.files(
  cache_dir,
  pattern = "^(glasgow_reuse|batch_.*)\\.rds$",
  full.names = TRUE
)
for (path in existing_batch_files) {
  batch <- readRDS(path)
  have_words <- c(have_words, names(batch))
}

have_words <- unique(have_words)
needed_words <- setdiff(target_words, have_words)
message(
  "Need embeddings for ", length(needed_words), " of ",
  length(target_words), " Lancaster words."
)

if (length(needed_words) > 0) {
  existing_ids <- as.integer(sub(
    "^batch_(\\d+)\\.rds$",
    "\\1",
    basename(list.files(cache_dir, pattern = "^batch_\\d+\\.rds$"))
  ))
  next_id <- if (length(existing_ids) == 0) 1L else max(existing_ids) + 1L
  num_batches <- ceiling(length(needed_words) / batch_size)

  for (i in seq_len(num_batches)) {
    start_index <- (i - 1L) * batch_size + 1L
    end_index <- min(i * batch_size, length(needed_words))
    batch_words <- needed_words[start_index:end_index]
    message(
      "Fetching batch ", i, " of ", num_batches,
      " (", start_index, "-", end_index, ")..."
    )

    batch_embeddings <- tryCatch(
      word_embeddings(
        texts = batch_words,
        provider = provider,
        model = openai_model
      ),
      error = function(e) {
        warning("Error processing batch ", i, ": ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(batch_embeddings) && length(batch_embeddings) > 0) {
      names(batch_embeddings) <- normalize_word(names(batch_embeddings))
      batch_path <- file.path(
        cache_dir,
        sprintf("batch_%04d.rds", next_id + i - 1L)
      )
      saveRDS(batch_embeddings, batch_path)
    } else {
      warning("Skipping empty or failed batch ", i, ".")
    }

    if (i < num_batches) {
      Sys.sleep(pause_seconds)
    }
  }
}

batch_files <- list.files(
  cache_dir,
  pattern = "^(glasgow_reuse|batch_.*)\\.rds$",
  full.names = TRUE
)
if (length(batch_files) == 0) {
  stop("No embedding batches found; nothing to combine.")
}

combined_list <- list()
for (path in batch_files) {
  batch <- readRDS(path)
  combined_list[names(batch)] <- batch
}

missing_words <- setdiff(target_words, names(combined_list))
if (length(missing_words) > 0) {
  warning(
    "Still missing embeddings for ", length(missing_words),
    " words. Combined cache was written with available rows only."
  )
}

keep_words <- intersect(target_words, names(combined_list))
embedding_dim <- length(combined_list[[keep_words[[1]]]])
embedding_matrix <- do.call(rbind, combined_list[keep_words])
rownames(embedding_matrix) <- keep_words

combined <- list(
  words = keep_words,
  matrix = embedding_matrix,
  provider = provider,
  model = openai_model,
  embedding_dimension = embedding_dim
)
saveRDS(combined, combined_path)

message(
  "Saved combined embeddings for ", length(keep_words),
  " words (", embedding_dim, " dims) to ", combined_path, "."
)
