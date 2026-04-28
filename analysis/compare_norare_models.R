#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(glmnet))
suppressPackageStartupMessages(library(xgboost))

load("data/imageability_training_frame.rda")
load("data/glasgow_embeddings.rda")

rsq <- function(actual, predicted) {
  if (length(actual) < 2 || var(actual) < 1e-8 || var(predicted) < 1e-8) {
    return(NA_real_)
  }
  cor(actual, predicted)^2
}

rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))
mae <- function(actual, predicted) mean(abs(actual - predicted))

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

overlap <- imageability_training_frame[
  imageability_training_frame$has_embedding &
    rowSums(!is.na(imageability_training_frame[, lexical_feature_cols, drop = FALSE])) > 0,
]

embedding_matrix <- do.call(rbind, glasgow_embeddings[overlap$word])
y <- overlap$imageability_m

diagnostic_cols <- c("word_length", "glasgow_imageability_n", "glasgow_context_count", "glasgow_variant_count")
lexical_matrix <- as.matrix(overlap[, c(lexical_feature_cols, missing_cols, diagnostic_cols)])
lexical_matrix_glmnet <- lexical_matrix
lexical_matrix_glmnet[is.na(lexical_matrix_glmnet)] <- 0

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

fit_xgboost_variant <- function(x_train, y_train, x_test) {
  dtrain <- xgb.DMatrix(data = x_train, label = y_train, missing = NA_real_)
  grid <- expand.grid(
    eta = c(0.03, 0.1),
    max_depth = c(4L, 6L),
    min_child_weight = c(1, 5),
    subsample = 0.8,
    colsample_bytree = c(0.6, 0.9),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  best <- NULL
  best_score <- Inf

  for (i in seq_len(nrow(grid))) {
    params <- list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      eta = grid$eta[[i]],
      max_depth = grid$max_depth[[i]],
      min_child_weight = grid$min_child_weight[[i]],
      subsample = grid$subsample[[i]],
      colsample_bytree = grid$colsample_bytree[[i]]
    )

    cv <- xgb.cv(
      params = params,
      data = dtrain,
      nrounds = 300,
      nfold = 3,
      early_stopping_rounds = 15,
      verbose = 0
    )

    best_iter <- cv$best_iteration
    if (is.null(best_iter) || length(best_iter) == 0 || is.na(best_iter)) {
      best_iter <- which.min(cv$evaluation_log$test_rmse_mean)
    }
    score <- cv$evaluation_log$test_rmse_mean[[best_iter]]
    if (score < best_score) {
      best_score <- score
      best <- list(
        params = params,
        nrounds = best_iter,
        score = score
      )
    }
  }

  model <- xgb.train(
    params = best$params,
    data = dtrain,
    nrounds = best$nrounds,
    verbose = 0
  )

  list(
    predictions = as.vector(predict(model, newdata = xgb.DMatrix(x_test, missing = NA_real_))),
    eta = best$params$eta,
    max_depth = best$params$max_depth,
    min_child_weight = best$params$min_child_weight,
    subsample = best$params$subsample,
    colsample_bytree = best$params$colsample_bytree,
    nrounds = best$nrounds,
    cv_rmse = best$score
  )
}

evaluate_variant <- function(fold_id, variant) {
  alpha_grid <- c(0, 0.25, 0.5, 0.75, 1)
  results <- vector("list", max(fold_id))

  make_result_row <- function(variant, fold, rsquared, rmse, mae,
                              alpha = NA_real_, lambda = NA_real_,
                              eta = NA_real_, max_depth = NA_real_,
                              min_child_weight = NA_real_, subsample = NA_real_,
                              colsample_bytree = NA_real_, nrounds = NA_real_,
                              cv_rmse = NA_real_) {
    data.frame(
      variant = variant,
      fold = fold,
      rsquared = rsquared,
      rmse = rmse,
      mae = mae,
      alpha = alpha,
      lambda = lambda,
      eta = eta,
      max_depth = max_depth,
      min_child_weight = min_child_weight,
      subsample = subsample,
      colsample_bytree = colsample_bytree,
      nrounds = nrounds,
      cv_rmse = cv_rmse
    )
  }

  for (fold in seq_len(max(fold_id))) {
    train_idx <- which(fold_id != fold)
    test_idx <- which(fold_id == fold)

    if (variant == "enet_embeddings_baseline") {
      x_train <- cbind(embedding_matrix[train_idx, , drop = FALSE], word_length = overlap$word_length[train_idx])
      x_test <- cbind(embedding_matrix[test_idx, , drop = FALSE], word_length = overlap$word_length[test_idx])
      fit <- fit_glmnet_variant(x_train, y[train_idx], x_test, alpha_grid)
      results[[fold]] <- make_result_row(
        variant = variant,
        fold = fold,
        rsquared = rsq(y[test_idx], fit$predictions),
        rmse = rmse(y[test_idx], fit$predictions),
        mae = mae(y[test_idx], fit$predictions),
        alpha = fit$alpha,
        lambda = fit$lambda
      )
    } else if (variant == "enet_embeddings_plus_lexical") {
      x_train <- cbind(
        embedding_matrix[train_idx, , drop = FALSE],
        lexical_matrix_glmnet[train_idx, , drop = FALSE]
      )
      x_test <- cbind(
        embedding_matrix[test_idx, , drop = FALSE],
        lexical_matrix_glmnet[test_idx, , drop = FALSE]
      )
      fit <- fit_glmnet_variant(x_train, y[train_idx], x_test, alpha_grid)
      results[[fold]] <- make_result_row(
        variant = variant,
        fold = fold,
        rsquared = rsq(y[test_idx], fit$predictions),
        rmse = rmse(y[test_idx], fit$predictions),
        mae = mae(y[test_idx], fit$predictions),
        alpha = fit$alpha,
        lambda = fit$lambda
      )
    } else if (variant == "xgb_lexical_only") {
      x_train <- lexical_matrix[train_idx, , drop = FALSE]
      x_test <- lexical_matrix[test_idx, , drop = FALSE]
      fit <- fit_xgboost_variant(x_train, y[train_idx], x_test)
      results[[fold]] <- make_result_row(
        variant = variant,
        fold = fold,
        rsquared = rsq(y[test_idx], fit$predictions),
        rmse = rmse(y[test_idx], fit$predictions),
        mae = mae(y[test_idx], fit$predictions),
        eta = fit$eta,
        max_depth = fit$max_depth,
        min_child_weight = fit$min_child_weight,
        subsample = fit$subsample,
        colsample_bytree = fit$colsample_bytree,
        nrounds = fit$nrounds,
        cv_rmse = fit$cv_rmse
      )
    } else if (variant == "xgb_pca_embeddings_plus_lexical") {
      pca_fit <- prcomp(embedding_matrix[train_idx, , drop = FALSE], center = TRUE, scale. = TRUE, rank. = 50)
      train_pcs <- predict(pca_fit, newdata = embedding_matrix[train_idx, , drop = FALSE])
      test_pcs <- predict(pca_fit, newdata = embedding_matrix[test_idx, , drop = FALSE])
      x_train <- cbind(train_pcs, lexical_matrix[train_idx, , drop = FALSE])
      x_test <- cbind(test_pcs, lexical_matrix[test_idx, , drop = FALSE])
      fit <- fit_xgboost_variant(x_train, y[train_idx], x_test)
      results[[fold]] <- make_result_row(
        variant = variant,
        fold = fold,
        rsquared = rsq(y[test_idx], fit$predictions),
        rmse = rmse(y[test_idx], fit$predictions),
        mae = mae(y[test_idx], fit$predictions),
        eta = fit$eta,
        max_depth = fit$max_depth,
        min_child_weight = fit$min_child_weight,
        subsample = fit$subsample,
        colsample_bytree = fit$colsample_bytree,
        nrounds = fit$nrounds,
        cv_rmse = fit$cv_rmse
      )
    } else {
      stop("Unknown variant: ", variant)
    }
  }

  do.call(rbind, results)
}

set.seed(42)
fold_id <- sample(rep(1:5, length.out = length(y)))
variants <- c(
  "enet_embeddings_baseline",
  "enet_embeddings_plus_lexical",
  "xgb_lexical_only",
  "xgb_pca_embeddings_plus_lexical"
)

all_metrics <- do.call(
  rbind,
  lapply(variants, function(variant) {
    message("Evaluating ", variant, "...")
    variant_metrics <- evaluate_variant(fold_id, variant)
    output_file <- paste0("analysis/norare_", variant, "_folds.csv")
    write.csv(variant_metrics, output_file, row.names = FALSE)
    variant_metrics
  })
)

summary_metrics <- aggregate(
  all_metrics[, c("rsquared", "rmse", "mae")],
  by = list(variant = all_metrics$variant),
  FUN = mean
)
summary_metrics <- summary_metrics[order(-summary_metrics$rsquared, summary_metrics$rmse), ]

print(summary_metrics)
write.csv(all_metrics, "analysis/norare_model_comparison_folds.csv", row.names = FALSE)
write.csv(summary_metrics, "analysis/norare_model_comparison_summary.csv", row.names = FALSE)

message("Saved fold metrics to analysis/norare_model_comparison_folds.csv")
message("Saved summary metrics to analysis/norare_model_comparison_summary.csv")
