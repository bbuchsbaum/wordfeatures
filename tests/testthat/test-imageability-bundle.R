test_that("internal bundle builds lexical fallback rows", {
  bundle <- wordfeatures:::.get_imageability_model_bundle()
  embeddings <- list(
    abdomen = rep(0.1, bundle$embedding_dimension),
    madeupword = rep(0.2, bundle$embedding_dimension)
  )

  predictor_info <- wordfeatures:::.build_predictor_matrix(
    words = c("abdomen", "madeupword"),
    embeddings_list = embeddings,
    bundle = bundle
  )

  expect_true(all(predictor_info$valid_indices))
  expect_equal(nrow(predictor_info$matrix), 2)
  expect_equal(
    ncol(predictor_info$matrix),
    bundle$embedding_dimension + length(bundle$lexical_feature_cols) +
      length(bundle$missing_indicator_cols) + 1
  )
})

test_that("predict_imageability works with mocked embeddings", {
  bundle <- wordfeatures:::.get_imageability_model_bundle()
  mock_embeddings <- function(texts, provider = "openai", ...) {
    out <- replicate(length(texts), rep(0.05, bundle$embedding_dimension), simplify = FALSE)
    names(out) <- texts
    out
  }

  testthat::local_mocked_bindings(
    word_embeddings = mock_embeddings,
    .package = "wordfeatures"
  )

  preds <- predict_imageability(c("abdomen", "madeupword"))
  expect_type(preds, "double")
  expect_length(preds, 2)
  expect_equal(names(preds), c("abdomen", "madeupword"))
  expect_true(all(is.finite(preds)))
})
