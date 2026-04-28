#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(glmnet))
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(quiet = TRUE)
}

dir.create("analysis/cache", recursive = TRUE, showWarnings = FALSE)

load("data/imageability_training_frame.rda")
load("data/glasgow_embeddings.rda")

normalize_word <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("\\s+", " ", x)
  x
}

rsq <- function(actual, predicted) {
  if (length(actual) < 2 || var(actual) < 1e-8 || var(predicted) < 1e-8) {
    return(NA_real_)
  }
  cor(actual, predicted)^2
}

rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))
mae <- function(actual, predicted) mean(abs(actual - predicted))

fit_glmnet_variant <- function(x_train, y_train, x_test, alpha_grid) {
  fits <- lapply(alpha_grid, function(alpha_val) {
    cv.glmnet(x_train, y_train, alpha = alpha_val, nfolds = 5)
  })
  min_errors <- vapply(fits, function(fit) min(fit$cvm), numeric(1))
  best_idx <- which.min(min_errors)
  best_fit <- fits[[best_idx]]

  list(
    predictions = as.vector(predict(best_fit, newx = x_test, s = "lambda.min")),
    alpha = alpha_grid[[best_idx]],
    lambda = best_fit$lambda.min
  )
}

fetch_embedding_cache <- function(words, model_name, provider = "openai", batch_size = 100) {
  cache_file <- file.path(
    "analysis",
    "cache",
    paste0("embeddings_", provider, "_", gsub("[^A-Za-z0-9]+", "_", model_name), ".rds")
  )

  if (file.exists(cache_file)) {
    cached <- readRDS(cache_file)
    if (all(words %in% names(cached))) {
      message("Using cached embeddings for ", provider, " / ", model_name)
      return(cached[words])
    }
  } else {
    cached <- list()
  }

  missing_words <- setdiff(words, names(cached))
  if (length(missing_words) == 0) {
    return(cached[words])
  }

  num_batches <- ceiling(length(missing_words) / batch_size)
  for (batch_idx in seq_len(num_batches)) {
    start_idx <- (batch_idx - 1L) * batch_size + 1L
    end_idx <- min(batch_idx * batch_size, length(missing_words))
    batch_words <- missing_words[start_idx:end_idx]

    message(
      "Fetching ", provider, " / ", model_name,
      " batch ", batch_idx, " of ", num_batches,
      " (", start_idx, "-", end_idx, ")"
    )

    if (provider == "openai") {
      batch_embeddings <- word_embeddings(
        texts = batch_words,
        provider = "openai",
        model = model_name
      )
    } else {
      batch_embeddings <- word_embeddings(
        texts = batch_words,
        provider = "google",
        google_model = model_name
      )
    }
    cached <- c(cached, batch_embeddings)
    saveRDS(cached, cache_file)
    Sys.sleep(1)
  }

  cached[words]
}

missing_cols <- grep("_missing$", names(imageability_training_frame), value = TRUE)
lexical_feature_cols <- setdiff(
  names(imageability_training_frame),
  c(
    "word",
    "imageability_m",
    "glasgow_imageability_n",
    "glasgow_context_count",
    "glasgow_variant_count",
    "word_length",
    "has_embedding",
    missing_cols
  )
)

# Exclude direct imageability predictors for a cleaner comparison.
lexical_feature_cols <- lexical_feature_cols[!grepl("^imageability_", lexical_feature_cols)]
missing_cols <- missing_cols[!grepl("^imageability_", sub("_missing$", "", missing_cols))]

overlap <- imageability_training_frame[
  imageability_training_frame$has_embedding &
    rowSums(!is.na(imageability_training_frame[, lexical_feature_cols, drop = FALSE])) > 0,
]

words <- normalize_word(overlap$word)
y <- overlap$imageability_m

lexical_matrix <- as.matrix(
  overlap[, c(
    lexical_feature_cols,
    missing_cols,
    "word_length",
    "glasgow_imageability_n",
    "glasgow_context_count",
    "glasgow_variant_count"
  )]
)
lexical_matrix[is.na(lexical_matrix)] <- 0

embedding_sources <- list(
  openai_large = glasgow_embeddings[words],
  openai_small = fetch_embedding_cache(words, model_name = "text-embedding-3-small")
)

if (nzchar(Sys.getenv("GOOGLE_API_KEY")) || nzchar(Sys.getenv("GEMINI_API_KEY"))) {
  embedding_sources$google_gemini_embedding_001 <- fetch_embedding_cache(
    words,
    model_name = "gemini-embedding-001",
    provider = "google"
  )
}

set.seed(42)
fold_id <- sample(rep(1:5, length.out = length(words)))
alpha_grid <- c(0, 0.25, 0.5, 0.75, 1)

evaluate_source <- function(source_name, embeddings_list) {
  X <- do.call(rbind, embeddings_list)
  variants <- c("embeddings_only", "embeddings_plus_lexical")
  rows <- list()

  for (variant in variants) {
    for (fold in 1:5) {
      train_idx <- which(fold_id != fold)
      test_idx <- which(fold_id == fold)

      x_train <- X[train_idx, , drop = FALSE]
      x_test <- X[test_idx, , drop = FALSE]

      if (variant == "embeddings_only") {
        x_train <- cbind(x_train, word_length = overlap$word_length[train_idx])
        x_test <- cbind(x_test, word_length = overlap$word_length[test_idx])
      } else {
        x_train <- cbind(x_train, lexical_matrix[train_idx, , drop = FALSE])
        x_test <- cbind(x_test, lexical_matrix[test_idx, , drop = FALSE])
      }

      fit <- fit_glmnet_variant(x_train, y[train_idx], x_test, alpha_grid)
      rows[[length(rows) + 1L]] <- data.frame(
        source = source_name,
        variant = variant,
        fold = fold,
        rsquared = rsq(y[test_idx], fit$predictions),
        rmse = rmse(y[test_idx], fit$predictions),
        mae = mae(y[test_idx], fit$predictions),
        alpha = fit$alpha,
        lambda = fit$lambda
      )
    }
  }

  do.call(rbind, rows)
}

all_metrics <- do.call(
  rbind,
  lapply(names(embedding_sources), function(source_name) {
    message("Evaluating source: ", source_name)
    evaluate_source(source_name, embedding_sources[[source_name]])
  })
)

summary_metrics <- aggregate(
  all_metrics[, c("rsquared", "rmse", "mae")],
  by = list(source = all_metrics$source, variant = all_metrics$variant),
  FUN = mean
)
summary_metrics <- summary_metrics[order(summary_metrics$variant, -summary_metrics$rsquared, summary_metrics$rmse), ]

print(summary_metrics)
write.csv(all_metrics, "analysis/embedding_model_benchmark_folds.csv", row.names = FALSE)
write.csv(summary_metrics, "analysis/embedding_model_benchmark_summary.csv", row.names = FALSE)

message("Saved fold metrics to analysis/embedding_model_benchmark_folds.csv")
message("Saved summary metrics to analysis/embedding_model_benchmark_summary.csv")
