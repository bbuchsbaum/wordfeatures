# Build lexical augmentation rows for imageability prediction. This is kept
# internal so the package can evolve the bundled artifact without changing the
# public API.
.normalize_imageability_words <- function(words) {
  .normalize_words(words)
}

.get_imageability_model_bundle <- function() {
  model_namespace <- asNamespace("wordfeatures")

  if (exists("imageability_model_bundle", envir = model_namespace, inherits = FALSE)) {
    return(get("imageability_model_bundle", envir = model_namespace))
  }

  if (!exists("imageability_glmnet_model", envir = model_namespace, inherits = FALSE)) {
    stop(
      "Internal imageability model not found. Has the package been loaded correctly, and was the training script run?"
    )
  }

  legacy_model <- get("imageability_glmnet_model", envir = model_namespace)
  list(
    model = legacy_model,
    use_word_length = isTRUE(attr(legacy_model, "use_word_length")),
    use_lexical_lookup = FALSE,
    lexical_feature_cols = character(),
    missing_indicator_cols = character(),
    lexical_lookup = data.frame(word = character(), stringsAsFactors = FALSE),
    embedding_provider = "openai",
    embedding_model = "text-embedding-3-large",
    target_construction = attr(legacy_model, "target_construction")
  )
}

.build_predictor_matrix <- function(words, embeddings_list, bundle) {
  valid_indices <- !vapply(
    embeddings_list,
    function(x) is.null(x) || any(is.na(x)),
    logical(1)
  )
  valid_embeddings_list <- embeddings_list[valid_indices]
  words_with_valid_embeddings <- words[valid_indices]

  if (length(valid_embeddings_list) == 0) {
    return(list(matrix = NULL, valid_indices = valid_indices))
  }

  predictor_matrix <- do.call(rbind, valid_embeddings_list)

  if (isTRUE(bundle$use_lexical_lookup)) {
    normalized_words <- .normalize_imageability_words(words_with_valid_embeddings)
    lexical_rows <- bundle$lexical_lookup[
      match(normalized_words, bundle$lexical_lookup$word),
      bundle$lexical_feature_cols,
      drop = FALSE
    ]
    if (nrow(lexical_rows) == 0) {
      lexical_rows <- as.data.frame(
        matrix(
          NA_real_,
          nrow = length(words_with_valid_embeddings),
          ncol = length(bundle$lexical_feature_cols),
          dimnames = list(NULL, bundle$lexical_feature_cols)
        )
      )
    }

    missing_matrix <- is.na(lexical_rows)
    lexical_rows[missing_matrix] <- 0
    lexical_rows <- as.matrix(lexical_rows)
    mode(lexical_rows) <- "numeric"

    missing_indicator_matrix <- matrix(
      as.numeric(missing_matrix),
      nrow = nrow(lexical_rows),
      ncol = ncol(lexical_rows),
      dimnames = list(NULL, bundle$missing_indicator_cols)
    )

    predictor_matrix <- cbind(
      predictor_matrix,
      lexical_rows,
      missing_indicator_matrix
    )
  }

  if (isTRUE(bundle$use_word_length)) {
    predictor_matrix <- cbind(
      predictor_matrix,
      word_length = nchar(words_with_valid_embeddings)
    )
  }

  list(matrix = predictor_matrix, valid_indices = valid_indices)
}
