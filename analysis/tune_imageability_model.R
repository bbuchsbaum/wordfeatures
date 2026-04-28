#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(glmnet))

load_data <- function() {
  load("data/glasgow_norms.rda")
  load("data/glasgow_embeddings.rda")

  common_words <- intersect(glasgow_norms$word, names(glasgow_embeddings))
  if (length(common_words) == 0) {
    stop("No common words found between glasgow_norms and glasgow_embeddings.")
  }

  matched_rows <- match(common_words, glasgow_norms$word)
  y <- glasgow_norms$imageability_m[matched_rows]
  keep <- !is.na(y)

  words <- common_words[keep]
  y <- y[keep]
  X <- do.call(rbind, glasgow_embeddings[words])
  word_length <- nchar(words)

  list(words = words, y = y, X = X, word_length = word_length)
}

rsq <- function(actual, predicted) {
  if (length(actual) < 2 || var(actual) < 1e-8 || var(predicted) < 1e-8) {
    return(NA_real_)
  }
  cor(actual, predicted)^2
}

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

fit_glmnet_variant <- function(x_train, y_train, x_test, alpha_grid) {
  fits <- lapply(alpha_grid, function(alpha_val) {
    cv.glmnet(x_train, y_train, alpha = alpha_val, nfolds = 5)
  })
  min_errors <- vapply(fits, function(fit) min(fit$cvm), numeric(1))
  best_idx <- which.min(min_errors)
  best_fit <- fits[[best_idx]]
  predictions <- as.vector(predict(best_fit, newx = x_test, s = "lambda.min"))

  list(
    predictions = predictions,
    alpha = alpha_grid[[best_idx]],
    lambda = best_fit$lambda.min,
    cvm = min_errors[[best_idx]]
  )
}

evaluate_variant <- function(dataset, fold_id, variant) {
  alpha_grid <- c(0, 0.25, 0.5, 0.75, 1)
  fold_metrics <- vector("list", max(fold_id))

  for (fold in seq_len(max(fold_id))) {
    train_idx <- which(fold_id != fold)
    test_idx <- which(fold_id == fold)

    x_train <- dataset$X[train_idx, , drop = FALSE]
    x_test <- dataset$X[test_idx, , drop = FALSE]
    y_train <- dataset$y[train_idx]
    y_test <- dataset$y[test_idx]

    if (variant == "baseline_raw_enet") {
      fit <- fit_glmnet_variant(
        x_train = x_train,
        y_train = y_train,
        x_test = x_test,
        alpha_grid = 0.5
      )
    } else if (variant == "tuned_raw_enet") {
      fit <- fit_glmnet_variant(
        x_train = x_train,
        y_train = y_train,
        x_test = x_test,
        alpha_grid = alpha_grid
      )
    } else if (variant %in% c("pca100_tuned_enet", "pca100_tuned_enet_plus_length")) {
      pca_fit <- prcomp(x_train, center = TRUE, scale. = TRUE, rank. = 100)
      x_train <- predict(pca_fit, newdata = x_train)
      x_test <- predict(pca_fit, newdata = x_test)

      if (variant == "pca100_tuned_enet_plus_length") {
        x_train <- cbind(x_train, word_length = dataset$word_length[train_idx])
        x_test <- cbind(x_test, word_length = dataset$word_length[test_idx])
      }

      fit <- fit_glmnet_variant(
        x_train = x_train,
        y_train = y_train,
        x_test = x_test,
        alpha_grid = alpha_grid
      )
    } else if (variant == "raw_enet_plus_length") {
      x_train <- cbind(x_train, word_length = dataset$word_length[train_idx])
      x_test <- cbind(x_test, word_length = dataset$word_length[test_idx])

      fit <- fit_glmnet_variant(
        x_train = x_train,
        y_train = y_train,
        x_test = x_test,
        alpha_grid = alpha_grid
      )
    } else {
      stop("Unknown variant: ", variant)
    }

    fold_metrics[[fold]] <- data.frame(
      variant = variant,
      fold = fold,
      rsquared = rsq(y_test, fit$predictions),
      rmse = rmse(y_test, fit$predictions),
      mae = mae(y_test, fit$predictions),
      alpha = fit$alpha,
      lambda = fit$lambda,
      cvm = fit$cvm
    )
  }

  do.call(rbind, fold_metrics)
}

summarize_metrics <- function(metrics_df) {
  aggregate(
    metrics_df[, c("rsquared", "rmse", "mae")],
    by = list(variant = metrics_df$variant),
    FUN = mean
  )
}

set.seed(42)
dataset <- load_data()
fold_id <- sample(rep(1:5, length.out = length(dataset$y)))
variants <- c(
  "baseline_raw_enet",
  "tuned_raw_enet",
  "raw_enet_plus_length",
  "pca100_tuned_enet",
  "pca100_tuned_enet_plus_length"
)

all_metrics <- do.call(
  rbind,
  lapply(variants, function(variant) {
    message("Evaluating ", variant, "...")
    evaluate_variant(dataset, fold_id, variant)
  })
)

summary_metrics <- summarize_metrics(all_metrics)
summary_metrics <- summary_metrics[order(-summary_metrics$rsquared, summary_metrics$rmse), ]

print(summary_metrics)
write.csv(all_metrics, "analysis/imageability_model_fold_metrics.csv", row.names = FALSE)
write.csv(summary_metrics, "analysis/imageability_model_summary.csv", row.names = FALSE)

message("Saved fold metrics to analysis/imageability_model_fold_metrics.csv")
message("Saved summary metrics to analysis/imageability_model_summary.csv")
