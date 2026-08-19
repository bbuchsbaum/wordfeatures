tiny_sensorimotor_bundle <- function() {
  set.seed(1)
  dimension_names <- wordfeatures:::.sensorimotor_dimension_names
  n <- 80
  p <- 16
  x <- matrix(rnorm(n * p), n, p)
  nrm <- sqrt(rowSums(x * x))
  x <- x / nrm
  center <- colMeans(x)
  x_c <- sweep(x, 2, center, "-")
  coefs <- matrix(rnorm(p * length(dimension_names), sd = 0.2), p, length(dimension_names))
  y <- 2.5 + x_c %*% coefs + matrix(rnorm(n * length(dimension_names), sd = 0.1), n)
  y <- pmin(pmax(y, 0), 5)
  model <- glmnet::glmnet(
    x_c,
    y,
    family = "mgaussian",
    alpha = 0,
    lambda = 0.05,
    standardize = FALSE
  )
  list(
    model = model,
    embedding_provider = "openai",
    embedding_model = "text-embedding-3-large",
    embedding_dimension = p,
    dimension_names = dimension_names,
    clip_range = c(0, 5),
    center = center,
    normalize_l2 = TRUE,
    calibration = vector("list", length(dimension_names)),
    training_rows = n
  )
}

test_that("derived scores follow the specificity identity", {
  dimension_names <- wordfeatures:::.sensorimotor_dimension_names
  strengths <- matrix(
    c(
      4, 1, 1, 0.2, 0.1, 0.5, 1, 0.4, 1.2, 0.8, 0.3,
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2
    ),
    nrow = 2,
    byrow = TRUE
  )
  derived <- wordfeatures:::.sensorimotor_derived_scores(strengths, dimension_names)

  expect_equal(derived$visual_strength, c(4, 2))
  expect_equal(
    derived$visual_specificity[[1]],
    4 - mean(c(1, 1, 0.2, 0.1, 0.5, 1, 0.4, 1.2, 0.8, 0.3))
  )
  expect_equal(derived$visual_specificity[[2]], 0)
  expect_equal(derived$dominant_modality, c("visual", "visual"))
  expect_equal(derived$general_sensorimotor_strength[[2]], 2)
  expect_equal(derived$modality_exclusivity[[1]], max(unlist(derived[1, paste0(dimension_names, "_specificity")])))
})

test_that("derived scores keep NA rows empty", {
  dimension_names <- wordfeatures:::.sensorimotor_dimension_names
  strengths <- matrix(NA_real_, nrow = 1, ncol = length(dimension_names))
  derived <- wordfeatures:::.sensorimotor_derived_scores(strengths, dimension_names)
  expect_true(all(is.na(derived$visual_strength)))
  expect_true(is.na(derived$dominant_modality))
  expect_true(is.na(derived$modality_exclusivity))
})

test_that("predict_sensorimotor works with a tiny mocked bundle", {
  bundle <- tiny_sensorimotor_bundle()
  mock_embeddings <- function(texts, provider = "openai", ...) {
    out <- replicate(
      length(texts),
      as.numeric(rep(0.05, bundle$embedding_dimension)),
      simplify = FALSE
    )
    names(out) <- texts
    out
  }

  testthat::local_mocked_bindings(
    .get_sensorimotor_model_bundle = function() bundle,
    word_embeddings = mock_embeddings,
    .package = "wordfeatures"
  )

  preds <- predict_sensorimotor(c("bell", "cinnamon"))
  expect_s3_class(preds, "data.frame")
  expect_equal(preds$text, c("bell", "cinnamon"))
  expect_equal(nrow(preds), 2)
  strength_cols <- paste0(bundle$dimension_names, "_strength")
  expect_true(all(preds[, strength_cols] >= 0))
  expect_true(all(preds[, strength_cols] <= 5))
  expect_true(all(preds$dominant_modality %in% bundle$dimension_names))
  expect_equal(
    preds$visual_specificity,
    preds$visual_strength - rowMeans(preds[, paste0(setdiff(bundle$dimension_names, "visual"), "_strength")])
  )
})

test_that("bundled sensorimotor probe scores mocked embeddings", {
  bundle <- wordfeatures:::.get_sensorimotor_model_bundle()
  mock_embeddings <- function(texts, provider = "openai", ...) {
    out <- replicate(
      length(texts),
      as.numeric(rep(0.05, bundle$embedding_dimension)),
      simplify = FALSE
    )
    names(out) <- texts
    out
  }

  testthat::local_mocked_bindings(
    word_embeddings = mock_embeddings,
    .package = "wordfeatures"
  )

  preds <- predict_sensorimotor(c("bell", "cinnamon"))
  expect_s3_class(preds, "data.frame")
  expect_equal(nrow(preds), 2)
  expect_true(all(is.finite(preds$visual_strength)))
  expect_true(all(preds$visual_strength >= 0 & preds$visual_strength <= 5))
  expect_true(all(preds$dominant_modality %in% bundle$dimension_names))
})

test_that("predict_sensorimotor accepts supplied embeddings and warns on sentences", {
  bundle <- tiny_sensorimotor_bundle()
  embeddings <- list(
    as.numeric(rep(0.1, bundle$embedding_dimension)),
    as.numeric(rep(0.2, bundle$embedding_dimension))
  )

  testthat::local_mocked_bindings(
    .get_sensorimotor_model_bundle = function() bundle,
    .package = "wordfeatures"
  )

  expect_warning(
    preds <- predict_sensorimotor(
      c("bell", "the old church bell rang"),
      embeddings = embeddings
    ),
    "uncalibrated"
  )
  expect_equal(nrow(preds), 2)
  expect_true(all(is.finite(preds$visual_strength)))
})
