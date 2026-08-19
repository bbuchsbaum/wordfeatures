#!/usr/bin/env Rscript
#
# Train the bundled multi-output ridge probe from frozen embeddings onto
# Lancaster sensorimotor ratings. Families are held out as groups; a
# locked family test set is never used for lambda search or calibration.

suppressPackageStartupMessages(library(glmnet))

combined_path <- file.path("data-raw", "cache", "sensorimotor_embeddings", "combined.rds")
if (!file.exists(combined_path)) {
  stop("Missing ", combined_path, ". Run data-raw/sensorimotor_embeddings.R first.")
}
if (!file.exists("data/sensorimotor_training_frame.rda")) {
  stop("Missing data/sensorimotor_training_frame.rda. Run data-raw/sensorimotor_training_frame.R first.")
}

load("data/sensorimotor_training_frame.rda")
cached <- readRDS(combined_path)

dimension_names <- c(
  "visual", "auditory", "haptic", "olfactory", "gustatory",
  "interoceptive", "hand_arm", "foot_leg", "head", "mouth", "torso"
)

l2_normalize_rows <- function(x) {
  nrm <- sqrt(rowSums(x * x))
  nrm[!is.finite(nrm) | nrm == 0] <- 1
  x / nrm
}

fit_isotonic <- function(predicted, observed) {
  ok <- is.finite(predicted) & is.finite(observed)
  predicted <- predicted[ok]
  observed <- observed[ok]
  if (length(predicted) < 20 || length(unique(predicted)) < 5) {
    return(NULL)
  }
  iso <- stats::isoreg(predicted, observed)
  ord <- order(predicted)
  x <- predicted[ord]
  y <- as.numeric(iso$yf)
  keep <- !duplicated(x, fromLast = TRUE)
  list(x = x[keep], y = y[keep])
}

apply_isotonic <- function(predicted, calib) {
  if (is.null(calib) || is.null(calib$x) || length(calib$x) < 2) {
    return(predicted)
  }
  as.numeric(stats::approx(calib$x, calib$y, xout = predicted, rule = 2, ties = "ordered")$y)
}

clip01_5 <- function(x) pmin(pmax(x, 0), 5)

rsq <- function(actual, predicted) {
  ok <- is.finite(actual) & is.finite(predicted)
  actual <- actual[ok]
  predicted <- predicted[ok]
  if (length(actual) < 2 || stats::var(actual) < 1e-8 || stats::var(predicted) < 1e-8) {
    return(NA_real_)
  }
  stats::cor(actual, predicted)^2
}

spearman_rho <- function(actual, predicted) {
  ok <- is.finite(actual) & is.finite(predicted)
  if (sum(ok) < 3) {
    return(NA_real_)
  }
  stats::cor(actual[ok], predicted[ok], method = "spearman")
}

mae <- function(actual, predicted) {
  mean(abs(actual - predicted), na.rm = TRUE)
}

mse <- function(actual, predicted) {
  mean((actual - predicted)^2, na.rm = TRUE)
}

calibration_slope <- function(actual, predicted) {
  ok <- is.finite(actual) & is.finite(predicted)
  if (sum(ok) < 3 || stats::var(predicted[ok]) < 1e-8) {
    return(NA_real_)
  }
  unname(stats::coef(stats::lm(actual[ok] ~ predicted[ok]))[[2]])
}

metric_row <- function(dimension, split, actual, predicted) {
  data.frame(
    dimension = dimension,
    split = split,
    r_squared = rsq(actual, predicted),
    spearman = spearman_rho(actual, predicted),
    mae = mae(actual, predicted),
    mse = mse(actual, predicted),
    calibration_slope = calibration_slope(actual, predicted),
    stringsAsFactors = FALSE
  )
}

common_words <- intersect(sensorimotor_training_frame$word, cached$words)
training_frame <- sensorimotor_training_frame[
  match(common_words, sensorimotor_training_frame$word),
  ,
  drop = FALSE
]
Y <- as.matrix(training_frame[, dimension_names, drop = FALSE])
X_raw <- cached$matrix[training_frame$word, , drop = FALSE]
X <- l2_normalize_rows(X_raw)

set.seed(42)
families <- unique(training_frame$family)
n_test_families <- max(1L, as.integer(round(0.2 * length(families))))
test_families <- sample(families, n_test_families)
is_test <- training_frame$family %in% test_families
train_idx <- which(!is_test)
test_idx <- which(is_test)

train_families <- unique(training_frame$family[train_idx])
n_folds <- min(5L, length(train_families))
family_fold <- sample(rep(seq_len(n_folds), length.out = length(train_families)))
names(family_fold) <- train_families
foldid <- unname(family_fold[training_frame$family[train_idx]])

X_train <- X[train_idx, , drop = FALSE]
Y_train <- Y[train_idx, , drop = FALSE]
center <- colMeans(X_train)
X_train_c <- sweep(X_train, 2, center, "-")
X_test_c <- sweep(X[test_idx, , drop = FALSE], 2, center, "-")

message(
  "Training rows: ", length(train_idx),
  " (", length(train_families), " families); test rows: ",
  length(test_idx), " (", n_test_families, " families)."
)

cv_fit <- cv.glmnet(
  X_train_c,
  Y_train,
  family = "mgaussian",
  alpha = 0,
  nlambda = 25,
  foldid = foldid,
  standardize = FALSE,
  type.measure = "mse"
)
best_lambda <- cv_fit$lambda.min
message("Selected lambda.min = ", format(best_lambda, digits = 5))

oof <- matrix(NA_real_, nrow = length(train_idx), ncol = length(dimension_names))
colnames(oof) <- dimension_names
for (fold in seq_len(n_folds)) {
  inner_train <- which(foldid != fold)
  inner_test <- which(foldid == fold)
  fold_fit <- glmnet(
    X_train_c[inner_train, , drop = FALSE],
    Y_train[inner_train, , drop = FALSE],
    family = "mgaussian",
    alpha = 0,
    lambda = best_lambda,
    standardize = FALSE
  )
  fold_pred <- predict(fold_fit, newx = X_train_c[inner_test, , drop = FALSE], type = "response")
  if (length(dim(fold_pred)) == 3) {
    fold_pred <- fold_pred[, , 1]
  }
  oof[inner_test, ] <- as.matrix(fold_pred)
}

calibration <- vector("list", length(dimension_names))
names(calibration) <- dimension_names
for (j in seq_along(dimension_names)) {
  raw_mae <- mae(Y_train[, j], oof[, j])
  calib <- fit_isotonic(oof[, j], Y_train[, j])
  calibrated <- apply_isotonic(oof[, j], calib)
  if (!is.null(calib) && mae(Y_train[, j], calibrated) < raw_mae) {
    calibration[[j]] <- calib
  } else {
    calibration[[j]] <- NULL
  }
}

final_model <- glmnet(
  X_train_c,
  Y_train,
  family = "mgaussian",
  alpha = 0,
  lambda = best_lambda,
  standardize = FALSE
)

raw_test <- predict(final_model, newx = X_test_c, type = "response")
if (length(dim(raw_test)) == 3) {
  raw_test <- raw_test[, , 1]
}
raw_test <- as.matrix(raw_test)
cal_test <- raw_test
for (j in seq_along(dimension_names)) {
  cal_test[, j] <- clip01_5(apply_isotonic(raw_test[, j], calibration[[j]]))
}
raw_test <- clip01_5(raw_test)

train_means <- colMeans(Y_train)
baseline_test <- matrix(
  train_means,
  nrow = length(test_idx),
  ncol = length(dimension_names),
  byrow = TRUE
)

metric_parts <- list()
for (j in seq_along(dimension_names)) {
  dim_name <- dimension_names[[j]]
  y_test <- Y[test_idx, j]
  metric_parts[[length(metric_parts) + 1]] <- metric_row(
    dim_name, "holdout_calibrated", y_test, cal_test[, j]
  )
  metric_parts[[length(metric_parts) + 1]] <- metric_row(
    dim_name, "holdout_raw", y_test, raw_test[, j]
  )
  metric_parts[[length(metric_parts) + 1]] <- metric_row(
    dim_name, "holdout_mean_baseline", y_test, baseline_test[, j]
  )
  metric_parts[[length(metric_parts) + 1]] <- metric_row(
    dim_name, "oof_raw", Y_train[, j], oof[, j]
  )
}

dimension_metrics <- do.call(rbind, metric_parts)
row.names(dimension_metrics) <- NULL

overall <- aggregate(
  cbind(r_squared, spearman, mae, mse, calibration_slope) ~ split,
  data = dimension_metrics,
  FUN = function(x) mean(x, na.rm = TRUE),
  na.action = stats::na.pass
)

sysdata_env <- new.env(parent = emptyenv())
load("R/sysdata.rda", envir = sysdata_env)
imageability_glmnet_model <- sysdata_env$imageability_glmnet_model
imageability_model_bundle <- sysdata_env$imageability_model_bundle

sensorimotor_model_bundle <- list(
  model = final_model,
  embedding_provider = cached$provider,
  embedding_model = cached$model,
  embedding_dimension = ncol(X),
  dimension_names = dimension_names,
  clip_range = c(0, 5),
  center = center,
  normalize_l2 = TRUE,
  calibration = calibration,
  training_rows = length(train_idx),
  grouping = "porter stem family; multi-word families are sorted stemmed tokens",
  holdout_families = sort(test_families),
  holdout_rows = length(test_idx),
  lambda = best_lambda,
  target_construction = "Lancaster sensorimotor means; embeddings-only multi-output ridge"
)

usethis::use_data(
  imageability_glmnet_model,
  imageability_model_bundle,
  sensorimotor_model_bundle,
  internal = TRUE,
  overwrite = TRUE
)

dir.create("analysis", showWarnings = FALSE)
write.csv(dimension_metrics, "analysis/sensorimotor_bundle_dimension_metrics.csv", row.names = FALSE)
write.csv(overall, "analysis/sensorimotor_bundle_overall.csv", row.names = FALSE)
write.csv(
  data.frame(
    family = sort(test_families),
    stringsAsFactors = FALSE
  ),
  "analysis/sensorimotor_bundle_holdout_families.csv",
  row.names = FALSE
)

message("Saved sensorimotor_model_bundle to R/sysdata.rda")
cal_row <- overall[overall$split == "holdout_calibrated", ]
base_row <- overall[overall$split == "holdout_mean_baseline", ]
message("Holdout calibrated mean R^2: ", round(cal_row$r_squared, 3),
        "  MAE: ", round(cal_row$mae, 3),
        "  MSE: ", round(cal_row$mse, 3))
message("Holdout mean-baseline MAE: ", round(base_row$mae, 3),
        "  MSE: ", round(base_row$mse, 3))
