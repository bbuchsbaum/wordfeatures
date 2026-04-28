#' Predict Imageability from Word Embeddings
#'
#' This function takes new words, gets their embeddings using the package's
#' `word_embeddings` function, augments them with lexical features when those
#' features are available in the bundled lookup table, and then uses a
#' pre-trained glmnet model (stored internally in the package) to predict
#' imageability scores.
#'
#' @param new_words A character vector of words or phrases for which to predict
#'   imageability.
#' @param embedding_provider A string specifying the API provider ("openai" or "google")
#'   to use for generating embeddings. Defaults to `"openai"`. Make sure the
#'   corresponding API key environment variable (`OPENAI_API_KEY`,
#'   `GOOGLE_API_KEY`, or `GEMINI_API_KEY`) is set.
#' @param embedding_model A string specifying the model to use for embeddings.
#'   Defaults depend on the provider (`"text-embedding-3-large"` for OpenAI,
#'   `"gemini-embedding-001"` for Google).
#' @param ... Additional arguments passed to the `word_embeddings` function (e.g.,
#'   `google_api_key`, `api_key`).
#'
#' @return A named numeric vector where names are the input `new_words` and
#'   values are the predicted imageability scores. The bundled model will use
#'   lexical feature augmentation when a matching word is available in the
#'   package lookup table, and it falls back gracefully when no lexical record
#'   exists. Returns `NULL` if embeddings cannot be generated. Returns `NA` for
#'   words where embeddings were successfully generated but prediction failed.
#' @export
#' @importFrom glmnet predict.glmnet
#'
#' @examples
#' \donttest{
#' # Ensure the appropriate API key is set, e.g.:
#' # Sys.setenv(OPENAI_API_KEY = "your-key-here")
#'
#' words <- c("cat", "dog", "theoretical", "abstraction")
#' predicted_scores <- predict_imageability(words, embedding_provider = "openai")
#' print(predicted_scores)
#'
#' # Example using OpenAI
#' # Sys.setenv(OPENAI_API_KEY = "your-openai-key-here")
#' predicted_scores_oai <- predict_imageability(
#'   words,
#'   embedding_provider = "openai",
#'   embedding_model = "text-embedding-3-large"
#' )
#' print(predicted_scores_oai)
#' }
predict_imageability <- function(new_words,
                                 embedding_provider = c("openai", "google"),
                                 embedding_model = NULL,
                                 ...) {
  if (!is.character(new_words) || length(new_words) == 0) {
    stop("'new_words' must be a non-empty character vector.")
  }

  embedding_provider <- match.arg(embedding_provider)

  if (is.null(embedding_model)) {
    embedding_model <- if (embedding_provider == "openai") {
      "text-embedding-3-large"
    } else {
      "gemini-embedding-001"
    }
  }

  bundle <- .get_imageability_model_bundle()
  imageability_glmnet_model <- bundle$model

  trained_provider <- bundle$embedding_provider
  trained_model <- bundle$embedding_model
  provider_mismatch <- !is.null(trained_provider) && !is.null(trained_model) &&
    (embedding_provider != trained_provider || embedding_model != trained_model)
  if (provider_mismatch) {
    warning(
      "The bundled imageability model was trained with ",
      trained_provider, " / ", trained_model,
      ". Predictions with ",
      embedding_provider, " / ", embedding_model,
      " may be degraded."
    )
  }

  embedding_args <- list(texts = new_words, provider = embedding_provider)
  if (embedding_provider == "openai") {
    embedding_args$model <- embedding_model
  } else {
    embedding_args$google_model <- embedding_model
  }
  embedding_args <- c(embedding_args, list(...))

  embeddings_list <- tryCatch(
    do.call(word_embeddings, embedding_args),
    error = function(e) {
      warning("Failed to get embeddings: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(embeddings_list) || length(embeddings_list) == 0) {
    warning("Could not generate embeddings for the input words.")
    return(NULL)
  }

  predictor_info <- .build_predictor_matrix(new_words, embeddings_list, bundle)
  valid_indices <- predictor_info$valid_indices
  predictor_matrix <- predictor_info$matrix

  if (is.null(predictor_matrix)) {
    warning("No valid embeddings were generated for any input word.")
    return(NULL)
  }
  words_with_valid_embeddings <- new_words[valid_indices]

  predictions_mat <- tryCatch(
    glmnet::predict.glmnet(imageability_glmnet_model, newx = predictor_matrix, type = "response"),
    error = function(e) {
      warning("glmnet prediction failed: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(predictions_mat)) {
    predicted_scores <- rep(NA_real_, length(new_words))
    names(predicted_scores) <- new_words
    return(predicted_scores)
  }

  predicted_scores_vector <- as.vector(predictions_mat)
  names(predicted_scores_vector) <- words_with_valid_embeddings

  final_predictions <- rep(NA_real_, length(new_words))
  names(final_predictions) <- new_words
  final_predictions[valid_indices] <- predicted_scores_vector

  final_predictions
}
