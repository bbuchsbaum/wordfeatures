#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(glmnet))

load("data/imageability_training_frame.rda")
load("data/glasgow_embeddings.rda")

feature_missing_cols <- grep("_missing$", names(imageability_training_frame), value = TRUE)
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
    feature_missing_cols
  )
)

# Do not train on direct imageability predictors from auxiliary norm tables.
lexical_feature_cols <- lexical_feature_cols[!grepl("^imageability_", lexical_feature_cols)]
missing_indicator_cols <- feature_missing_cols[
  !grepl("^imageability_", sub("_missing$", "", feature_missing_cols))
]

training_frame <- imageability_training_frame[imageability_training_frame$has_embedding, ]
training_words <- training_frame$word
y <- training_frame$imageability_m

embedding_matrix <- do.call(rbind, glasgow_embeddings[training_words])
lexical_matrix <- as.matrix(training_frame[, c(lexical_feature_cols, missing_indicator_cols)])
lexical_matrix[is.na(lexical_matrix)] <- 0
word_length <- training_frame$word_length

predictor_matrix <- cbind(
  embedding_matrix,
  lexical_matrix,
  word_length = word_length
)

alpha_grid <- c(0, 0.25, 0.5, 0.75, 1)

fit_glmnet_variant <- function(x_train, y_train, x_test) {
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

rsq <- function(actual, predicted) {
  if (length(actual) < 2 || var(actual) < 1e-8 || var(predicted) < 1e-8) {
    return(NA_real_)
  }
  cor(actual, predicted)^2
}

rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))
mae <- function(actual, predicted) mean(abs(actual - predicted))

set.seed(42)
fold_id <- sample(rep(1:5, length.out = length(y)))
fold_metrics <- vector("list", max(fold_id))

for (fold in seq_len(max(fold_id))) {
  train_idx <- which(fold_id != fold)
  test_idx <- which(fold_id == fold)

  fit <- fit_glmnet_variant(
    x_train = predictor_matrix[train_idx, , drop = FALSE],
    y_train = y[train_idx],
    x_test = predictor_matrix[test_idx, , drop = FALSE]
  )

  fold_metrics[[fold]] <- data.frame(
    fold = fold,
    alpha = fit$alpha,
    lambda = fit$lambda,
    rsquared = rsq(y[test_idx], fit$predictions),
    rmse = rmse(y[test_idx], fit$predictions),
    mae = mae(y[test_idx], fit$predictions)
  )
}

fold_metrics <- do.call(rbind, fold_metrics)
mean_alpha <- mean(fold_metrics$alpha)
mean_lambda <- mean(fold_metrics$lambda)

imageability_glmnet_model <- glmnet(
  predictor_matrix,
  y,
  alpha = mean_alpha,
  lambda = mean_lambda
)
attr(imageability_glmnet_model, "use_word_length") <- TRUE
attr(imageability_glmnet_model, "use_lexical_lookup") <- TRUE
attr(imageability_glmnet_model, "target_construction") <- "mean imageability per word across duplicated contexts"

lexical_lookup <- training_frame[, c("word", lexical_feature_cols), drop = FALSE]
row.names(lexical_lookup) <- NULL

imageability_model_bundle <- list(
  model = imageability_glmnet_model,
  use_word_length = TRUE,
  use_lexical_lookup = TRUE,
  lexical_feature_cols = lexical_feature_cols,
  missing_indicator_cols = missing_indicator_cols,
  lexical_lookup = lexical_lookup,
  embedding_provider = "openai",
  embedding_model = "text-embedding-3-large",
  target_construction = "mean imageability per word across duplicated contexts",
  training_rows = nrow(training_frame),
  embedding_dimension = ncol(embedding_matrix)
)

usethis::use_data(
  imageability_glmnet_model,
  imageability_model_bundle,
  internal = TRUE,
  overwrite = TRUE
)

summary_metrics <- data.frame(
  target = "imageability_m",
  observations = nrow(training_frame),
  embedding_dimension = ncol(embedding_matrix),
  lexical_feature_count = length(lexical_feature_cols),
  missing_indicator_count = length(missing_indicator_cols),
  mean_alpha = mean_alpha,
  mean_lambda = mean_lambda,
  mean_rsquared = mean(fold_metrics$rsquared, na.rm = TRUE),
  mean_rmse = mean(fold_metrics$rmse, na.rm = TRUE),
  mean_mae = mean(fold_metrics$mae, na.rm = TRUE)
)

dir.create("analysis", showWarnings = FALSE)
write.csv(fold_metrics, "analysis/imageability_bundle_fold_metrics.csv", row.names = FALSE)
write.csv(summary_metrics, "analysis/imageability_bundle_summary.csv", row.names = FALSE)

message("Saved imageability_model_bundle and imageability_glmnet_model to R/sysdata.rda")
message("Saved training metrics to analysis/imageability_bundle_summary.csv")
