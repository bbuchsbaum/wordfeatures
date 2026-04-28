#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(glmnet))

load("data/glasgow_norms.rda")
load("data/glasgow_embeddings.rda")

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

make_dataset_first_match <- function() {
  common_words <- intersect(glasgow_norms$word, names(glasgow_embeddings))
  matched_rows <- match(common_words, glasgow_norms$word)
  y <- glasgow_norms$imageability_m[matched_rows]
  keep <- !is.na(y)

  words <- common_words[keep]
  list(
    name = "first_match",
    words = words,
    y = y[keep],
    X = do.call(rbind, glasgow_embeddings[words]),
    word_length = nchar(words)
  )
}

make_dataset_averaged <- function() {
  non_missing <- !is.na(glasgow_norms$imageability_m)
  norm_subset <- glasgow_norms[non_missing, c("word", "imageability_m")]
  avg_df <- aggregate(imageability_m ~ word, data = norm_subset, FUN = mean)
  common_words <- intersect(avg_df$word, names(glasgow_embeddings))
  matched_rows <- match(common_words, avg_df$word)
  y <- avg_df$imageability_m[matched_rows]

  list(
    name = "averaged_duplicates",
    words = common_words,
    y = y,
    X = do.call(rbind, glasgow_embeddings[common_words]),
    word_length = nchar(common_words)
  )
}

evaluate_dataset <- function(dataset) {
  set.seed(42)
  fold_id <- sample(rep(1:5, length.out = length(dataset$y)))
  alpha_grid <- c(0, 0.25, 0.5, 0.75, 1)

  variants <- c("raw", "raw_plus_length")
  all_metrics <- list()

  for (variant in variants) {
    fold_metrics <- vector("list", 5)
    for (fold in 1:5) {
      train_idx <- which(fold_id != fold)
      test_idx <- which(fold_id == fold)

      x_train <- dataset$X[train_idx, , drop = FALSE]
      x_test <- dataset$X[test_idx, , drop = FALSE]
      if (variant == "raw_plus_length") {
        x_train <- cbind(x_train, word_length = dataset$word_length[train_idx])
        x_test <- cbind(x_test, word_length = dataset$word_length[test_idx])
      }

      fit <- fit_glmnet_variant(x_train, dataset$y[train_idx], x_test, alpha_grid)
      fold_metrics[[fold]] <- data.frame(
        dataset = dataset$name,
        variant = variant,
        fold = fold,
        rsquared = rsq(dataset$y[test_idx], fit$predictions),
        rmse = rmse(dataset$y[test_idx], fit$predictions),
        mae = mae(dataset$y[test_idx], fit$predictions),
        alpha = fit$alpha,
        lambda = fit$lambda
      )
    }
    all_metrics[[variant]] <- do.call(rbind, fold_metrics)
  }

  do.call(rbind, all_metrics)
}

all_metrics <- rbind(
  evaluate_dataset(make_dataset_first_match()),
  evaluate_dataset(make_dataset_averaged())
)

summary_metrics <- aggregate(
  all_metrics[, c("rsquared", "rmse", "mae", "alpha", "lambda")],
  by = list(dataset = all_metrics$dataset, variant = all_metrics$variant),
  FUN = mean
)

summary_metrics <- summary_metrics[order(-summary_metrics$rsquared, summary_metrics$rmse), ]
print(summary_metrics)

write.csv(all_metrics, "analysis/imageability_target_comparison_folds.csv", row.names = FALSE)
write.csv(summary_metrics, "analysis/imageability_target_comparison_summary.csv", row.names = FALSE)
