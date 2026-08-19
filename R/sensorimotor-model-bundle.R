.sensorimotor_dimension_names <- c(
  "visual",
  "auditory",
  "haptic",
  "olfactory",
  "gustatory",
  "interoceptive",
  "hand_arm",
  "foot_leg",
  "head",
  "mouth",
  "torso"
)

.get_sensorimotor_model_bundle <- function() {
  model_namespace <- asNamespace("wordfeatures")
  if (exists("sensorimotor_model_bundle", envir = model_namespace, inherits = FALSE)) {
    return(get("sensorimotor_model_bundle", envir = model_namespace))
  }

  stop(
    "Internal sensorimotor model not found. Has the package been loaded correctly, ",
    "and was analysis/train_sensorimotor_model_bundle.R run?"
  )
}

.l2_normalize_rows <- function(x) {
  nrm <- sqrt(rowSums(x * x))
  nrm[!is.finite(nrm) | nrm == 0] <- 1
  x / nrm
}

.preprocess_sensorimotor_embeddings <- function(embedding_matrix, center) {
  x <- .l2_normalize_rows(as.matrix(embedding_matrix))
  if (!is.null(center)) {
    x <- sweep(x, 2, center, "-")
  }
  x
}

.apply_isotonic_calibration <- function(predictions, calibration) {
  calibrated <- as.matrix(predictions)
  if (is.null(calibration) || length(calibration) == 0) {
    return(calibrated)
  }

  for (j in seq_len(ncol(calibrated))) {
    calib <- calibration[[j]]
    if (is.null(calib) || is.null(calib$x) || length(calib$x) < 2) {
      next
    }
    calibrated[, j] <- as.numeric(
      stats::approx(
        calib$x,
        calib$y,
        xout = calibrated[, j],
        rule = 2,
        ties = "ordered"
      )$y
    )
  }
  calibrated
}

.clip_sensorimotor_strengths <- function(mat, clip_range = c(0, 5)) {
  mat <- as.matrix(mat)
  mat[] <- pmin(pmax(mat, clip_range[[1]]), clip_range[[2]])
  mat
}

.sensorimotor_derived_scores <- function(strength_matrix, dimension_names) {
  strength_matrix <- as.matrix(strength_matrix)
  n <- nrow(strength_matrix)
  m <- ncol(strength_matrix)
  if (m != length(dimension_names)) {
    stop("strength_matrix columns do not match dimension_names.")
  }

  finite_rows <- rowSums(is.finite(strength_matrix)) == m
  specificity <- matrix(NA_real_, nrow = n, ncol = m)
  general <- rep(NA_real_, n)
  exclusivity <- rep(NA_real_, n)
  dominant <- rep(NA_character_, n)

  if (any(finite_rows)) {
    sm <- strength_matrix[finite_rows, , drop = FALSE]
    row_sums <- rowSums(sm)
    spec <- sm - (row_sums - sm) / (m - 1)
    specificity[finite_rows, ] <- spec
    general[finite_rows] <- row_sums / m
    dominant[finite_rows] <- dimension_names[max.col(sm, ties.method = "first")]
    exclusivity[finite_rows] <- spec[cbind(seq_len(nrow(spec)), max.col(spec, ties.method = "first"))]
  }

  colnames(strength_matrix) <- paste0(dimension_names, "_strength")
  colnames(specificity) <- paste0(dimension_names, "_specificity")

  out <- cbind(
    as.data.frame(strength_matrix, stringsAsFactors = FALSE),
    as.data.frame(specificity, stringsAsFactors = FALSE)
  )
  out$general_sensorimotor_strength <- general
  out$dominant_modality <- dominant
  out$modality_exclusivity <- exclusivity
  out
}

.embeddings_list_to_matrix <- function(embeddings_list) {
  valid_indices <- !vapply(
    embeddings_list,
    function(x) is.null(x) || length(x) == 0 || any(is.na(x)),
    logical(1)
  )
  if (!any(valid_indices)) {
    return(list(matrix = NULL, valid_indices = valid_indices))
  }

  embedding_matrix <- do.call(rbind, embeddings_list[valid_indices])
  list(matrix = embedding_matrix, valid_indices = valid_indices)
}

.resolve_sensorimotor_embeddings <- function(texts, embeddings) {
  if (is.null(embeddings)) {
    return(NULL)
  }
  if (!is.list(embeddings)) {
    stop("'embeddings' must be a list of numeric vectors.")
  }

  if (length(embeddings) == length(texts)) {
    return(embeddings)
  }

  if (is.null(names(embeddings))) {
    stop("'embeddings' must have one vector per input text, or names matching the texts.")
  }

  idx <- match(.normalize_words(texts), .normalize_words(names(embeddings)))
  if (anyNA(idx)) {
    stop("'embeddings' must have one vector per input text, or names matching the texts.")
  }
  embeddings[idx]
}

.predict_sensorimotor_from_matrix <- function(predictor_matrix, bundle) {
  predictions <- predict(bundle$model, newx = predictor_matrix, type = "response")
  if (length(dim(predictions)) == 3) {
    predictions <- predictions[, , 1]
  }
  predictions <- as.matrix(predictions)
  if (ncol(predictions) != length(bundle$dimension_names)) {
    stop("Bundled sensorimotor model did not return one column per dimension.")
  }
  colnames(predictions) <- bundle$dimension_names

  predictions <- .apply_isotonic_calibration(predictions, bundle$calibration)
  .clip_sensorimotor_strengths(predictions, bundle$clip_range)
}
