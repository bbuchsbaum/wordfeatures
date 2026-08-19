#' Predict Sensorimotor Profiles from Word Embeddings
#'
#' Decode a frozen language-embedding vector into the 11 Lancaster
#' sensorimotor dimensions with a bundled multi-output ridge probe, then
#' attach specificity, general magnitude, dominant modality, and
#' exclusivity scores.
#'
#' The bundled model is embeddings-only. It estimates the human-rated
#' sensory and action associations that are linearly decodable from the
#' frozen representation; it does not claim that the embedding model has
#' human-like sensory grounding. Sentence-length inputs can be scored as
#' an exploratory zero-shot baseline, but those values are not
#' calibrated.
#'
#' @param texts A character vector of words or phrases.
#' @param embedding_provider A string specifying the API provider
#'   (`"openai"` or `"google"`). Defaults to `"openai"`.
#' @param embedding_model A string specifying the embedding model.
#'   Defaults depend on the provider (`"text-embedding-3-large"` for
#'   OpenAI, `"gemini-embedding-001"` for Google).
#' @param embeddings Optional list of numeric embedding vectors. When
#'   supplied, the embedding API is not called. The list may be aligned
#'   with `texts` by position or by name.
#' @param ... Additional arguments passed to [word_embeddings()] when
#'   `embeddings` is `NULL`.
#'
#' @return A data frame with one row per input text and columns:
#'   `text`; 11 `*_strength` ratings on the Lancaster 0-5 scale; 11
#'   `*_specificity` scores (`strength_m - mean of the other strengths`);
#'   `general_sensorimotor_strength`; `dominant_modality`; and
#'   `modality_exclusivity` (the largest specificity). Rows for failed
#'   embeddings are `NA`.
#'
#' @export
#' @importFrom stats predict
#'
#' @examples
#' \donttest{
#' # Sys.setenv(OPENAI_API_KEY = "your-key-here")
#' predict_sensorimotor(c("bell", "cinnamon", "justice"))
#' }
predict_sensorimotor <- function(texts,
                                 embedding_provider = c("openai", "google"),
                                 embedding_model = NULL,
                                 embeddings = NULL,
                                 ...) {
  if (!is.character(texts) || length(texts) == 0) {
    stop("'texts' must be a non-empty character vector.")
  }

  embedding_provider <- match.arg(embedding_provider)
  if (is.null(embedding_model)) {
    embedding_model <- if (embedding_provider == "openai") {
      "text-embedding-3-large"
    } else {
      "gemini-embedding-001"
    }
  }

  n_tokens <- vapply(
    strsplit(trimws(texts), "\\s+"),
    function(tokens) sum(nzchar(tokens)),
    integer(1)
  )
  if (any(n_tokens >= 4)) {
    warning(
      "The lexical sensorimotor probe is uncalibrated for sentence-length ",
      "inputs. Predictions for texts with 4 or more tokens are exploratory."
    )
  }

  bundle <- .get_sensorimotor_model_bundle()
  embeddings_list <- .resolve_sensorimotor_embeddings(texts, embeddings)

  if (is.null(embeddings_list)) {
    trained_provider <- bundle$embedding_provider
    trained_model <- bundle$embedding_model
    provider_mismatch <- !is.null(trained_provider) && !is.null(trained_model) &&
      (embedding_provider != trained_provider || embedding_model != trained_model)
    if (provider_mismatch) {
      warning(
        "The bundled sensorimotor model was trained with ",
        trained_provider, " / ", trained_model,
        ". Predictions with ",
        embedding_provider, " / ", embedding_model,
        " may be degraded."
      )
    }

    embedding_args <- list(texts = texts, provider = embedding_provider)
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
  }

  if (is.null(embeddings_list) || length(embeddings_list) == 0) {
    warning("Could not generate embeddings for the input texts.")
    return(.empty_sensorimotor_predictions(texts, bundle$dimension_names))
  }

  embedding_info <- .embeddings_list_to_matrix(embeddings_list)
  if (is.null(embedding_info$matrix)) {
    warning("No valid embeddings were generated for any input text.")
    return(.empty_sensorimotor_predictions(texts, bundle$dimension_names))
  }

  if (ncol(embedding_info$matrix) != bundle$embedding_dimension) {
    stop(
      "Embedding dimension is ", ncol(embedding_info$matrix),
      " but the bundled sensorimotor model expects ",
      bundle$embedding_dimension, "."
    )
  }

  predictor_matrix <- .preprocess_sensorimotor_embeddings(
    embedding_info$matrix,
    bundle$center
  )

  predicted_strengths <- tryCatch(
    .predict_sensorimotor_from_matrix(predictor_matrix, bundle),
    error = function(e) {
      warning("Sensorimotor prediction failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(predicted_strengths)) {
    return(.empty_sensorimotor_predictions(texts, bundle$dimension_names))
  }

  strength_matrix <- matrix(
    NA_real_,
    nrow = length(texts),
    ncol = length(bundle$dimension_names)
  )
  strength_matrix[embedding_info$valid_indices, ] <- predicted_strengths
  derived <- .sensorimotor_derived_scores(strength_matrix, bundle$dimension_names)
  data.frame(text = texts, derived, stringsAsFactors = FALSE)
}

.empty_sensorimotor_predictions <- function(texts, dimension_names) {
  strength_matrix <- matrix(NA_real_, nrow = length(texts), ncol = length(dimension_names))
  derived <- .sensorimotor_derived_scores(strength_matrix, dimension_names)
  data.frame(text = texts, derived, stringsAsFactors = FALSE)
}
