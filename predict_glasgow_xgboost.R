# predict_glasgow_xgboost.R

# --- 1. Load Libraries ---
# Ensure required packages are installed
# install.packages(c("xgboost", "caret", "dplyr"))
library(xgboost)
library(caret)
library(dplyr)

message("Libraries loaded.")

# --- 2. Load Data ---
# Load the wordfeatures package environment
# (Assuming it's installed or loaded via devtools::load_all() beforehand)
if (!requireNamespace("wordfeatures", quietly = TRUE)) {
  if (requireNamespace("devtools", quietly = TRUE) && interactive()) {
      message("Loading package 'wordfeatures' using devtools::load_all()")
      devtools::load_all()
  } else {
     stop("Could not load the 'wordfeatures' package. Please install or load it.")
  }
} else {
    library(wordfeatures) # Load if installed
}


# Load datasets from the package
data("glasgow_norms", package = "wordfeatures", envir = environment())
data("glasgow_embeddings", package = "wordfeatures", envir = environment())

# Check if data loaded
if (!exists("glasgow_norms") || !exists("glasgow_embeddings")) {
  stop("Required datasets ('glasgow_norms', 'glasgow_embeddings') not found. Ensure they are generated and the package is loaded.")
}
message("Data loaded.")

# --- 3. Prepare Data ---
# (This section is largely the same as predict_glasgow.R)

# Define target variables (mean ratings)
target_vars <- c("arousal_m", "valence_m", "dominance_m", "concreteness_m",
                 "imageability_m", "familiarity_m", "aoa_m", "gender_m", "size_m")

# Filter glasgow_norms to ensure target variables exist
target_vars <- intersect(target_vars, colnames(glasgow_norms))
if (length(target_vars) == 0) {
    stop("None of the specified target variables found in glasgow_norms.")
}
message("Target variables identified: ", paste(target_vars, collapse=", "))

# Convert embeddings list to matrix
words_in_embeddings <- names(glasgow_embeddings)
if (length(words_in_embeddings) == 0) {
    stop("glasgow_embeddings is empty or has no names.")
}
emb_dim <- unique(sapply(glasgow_embeddings, length))
if (length(emb_dim) > 1) stop("Embeddings have inconsistent dimensions.")
if (emb_dim[1] == 0) stop("Embeddings have zero dimensions.")
embedding_matrix <- do.call(rbind, glasgow_embeddings)
rownames(embedding_matrix) <- words_in_embeddings
message("Embedding matrix created with dimensions: ", nrow(embedding_matrix), " x ", ncol(embedding_matrix))

# Align norms and embeddings
common_words <- intersect(glasgow_norms$word, words_in_embeddings)
if (length(common_words) == 0) stop("No common words found between glasgow_norms and glasgow_embeddings.")

glasgow_norms_aligned <- glasgow_norms %>%
  filter(word %in% common_words) %>% 
  arrange(word)

embedding_matrix_aligned <- embedding_matrix[glasgow_norms_aligned$word, ]
message("Aligned norms and embeddings. Number of observations: ", length(common_words))

# Select target columns and handle potential NAs
Y_full <- glasgow_norms_aligned %>% select(all_of(target_vars))
complete_target_rows <- complete.cases(Y_full)
if(sum(!complete_target_rows) > 0) message("Removing ", sum(!complete_target_rows), " rows with NA values in target variables.")

Y <- as.matrix(Y_full[complete_target_rows, ])
X <- embedding_matrix_aligned[complete_target_rows, ]
aligned_words <- glasgow_norms_aligned$word[complete_target_rows]

if(nrow(Y) < 20) stop("Too few complete observations remaining after NA removal.")
message("Final data prepared. X dimensions: ", nrow(X), " x ", ncol(X), ", Y dimensions: ", nrow(Y), " x ", ncol(Y))


# --- 4. Set up Outer Cross-Validation ---
k_outer_folds <- 10
set.seed(456) # Use a different seed for reproducibility
outer_cv_folds <- caret::createFolds(1:nrow(Y), k = k_outer_folds, list = TRUE, returnTrain = FALSE)
message(paste0("Created ", k_outer_folds, "-fold outer cross-validation indices."))

# --- 5. Initialize Results Storage ---
# Store predictions and actuals per fold, per target
fold_results <- vector("list", k_outer_folds)
names(fold_results) <- paste0("Fold", 1:k_outer_folds)
for(i in 1:k_outer_folds) {
    fold_results[[i]] <- vector("list", length(target_vars))
    names(fold_results[[i]]) <- target_vars
}
best_hyperparams <- vector("list", k_outer_folds) # Store best params per fold/target

# --- 6. Nested Cross-Validation Loop ---

# Define Inner CV settings (used within caret::train)
train_control_inner <- trainControl(
  method = "cv",
  number = 5, # 5-fold inner CV
  verboseIter = FALSE,
  allowParallel = TRUE # Set to FALSE if not using parallel backend
)

# Define XGBoost tuning grid (example - adjust ranges as needed)
xgb_grid <- expand.grid(
  nrounds = c(50, 100, 150),        # Number of boosting rounds
  max_depth = c(3, 5, 7),         # Max tree depth
  eta = c(0.1, 0.3),             # Learning rate
  gamma = 0,                      # Minimum loss reduction
  colsample_bytree = 0.8,         # Subsample ratio of columns
  min_child_weight = 1,           # Minimum sum of instance weight
  subsample = 0.8                 # Subsample ratio of the training instance
)

# --- Outer Loop ---
for (i in 1:k_outer_folds) {
  message(paste("Processing Outer Fold", i, "of", k_outer_folds, "..."))

  test_indices <- outer_cv_folds[[i]]
  train_indices <- setdiff(1:nrow(Y), test_indices)

  X_train <- X[train_indices, ]
  Y_train <- Y[train_indices, ]
  X_test <- X[test_indices, ]
  Y_test <- Y[test_indices, ]

  fold_best_params <- list()

  # --- Inner Loop (Target Variables) ---
  for (target_var in target_vars) {
    message(paste("  Training for target:", target_var))
    Y_train_target <- Y_train[, target_var]
    Y_test_target <- Y_test[, target_var]

    # Use tryCatch for caret::train
    model_fit <- tryCatch({
        suppressWarnings( # Suppress warnings during tuning (e.g., about standard deviations)
            caret::train(
                x = X_train,
                y = Y_train_target,
                method = "xgbTree",
                trControl = train_control_inner,
                tuneGrid = xgb_grid,
                verbose = 0 # Reduce xgboost verbosity within caret
                # Pass other xgboost params directly if needed, e.g., objective = 'reg:squarederror'
            )
        )
    }, error = function(e) {
        warning("caret::train failed for fold ", i, ", target ", target_var, "': ", e$message)
        return(NULL)
    })

    if (is.null(model_fit)) {
        fold_results[[i]][[target_var]] <- list(preds = NULL, actuals = Y_test_target)
        fold_best_params[[target_var]] <- NULL
        next # Skip to next target
    }

    fold_best_params[[target_var]] <- model_fit$bestTune
    message(paste("    Best Tune:", paste(names(model_fit$bestTune), model_fit$bestTune, sep="=", collapse=", ")))

    # Predict on outer test set
    predictions <- tryCatch({
        predict(model_fit, newdata = X_test)
    }, error = function(e) {
        warning("Prediction failed for fold ", i, ", target ", target_var, "': ", e$message)
        return(NULL)
    })

    fold_results[[i]][[target_var]] <- list(preds = predictions, actuals = Y_test_target)
  } # End inner loop (targets)
  best_hyperparams[[i]] <- fold_best_params
  message(paste("  Outer Fold", i, "completed."))
} # End outer loop

# --- 7. Aggregate and Report Results ---
message("\n--- Cross-Validation Results (XGBoost) ---")

overall_metrics <- list()

for (target_var in target_vars) {
    # Combine actuals and predictions for this target across all folds
    actuals_combined <- unlist(lapply(fold_results, function(fold) fold[[target_var]]$actuals))
    preds_combined <- unlist(lapply(fold_results, function(fold) fold[[target_var]]$preds))

    # Remove NAs that might have resulted from failed predictions/folds
    valid_indices <- !is.null(preds_combined) & !is.na(preds_combined) & !is.null(actuals_combined) & !is.na(actuals_combined)

    if (sum(valid_indices) < 2) {
        overall_metrics[[target_var]] <- list(Rsquared = NA, RMSE = NA, MAE = NA)
        next
    }

    actuals_valid <- actuals_combined[valid_indices]
    preds_valid <- preds_combined[valid_indices]


    # Calculate metrics if variance is sufficient
    rsq_val <- NA
    rmse_val <- NA
    mae_val <- NA

    if(length(actuals_valid) > 1 && var(actuals_valid) > 1e-8 && var(preds_valid) > 1e-8) {
        rsq_val <- cor(actuals_valid, preds_valid)^2
        rmse_val <- sqrt(mean((actuals_valid - preds_valid)^2))
        mae_val <- mean(abs(actuals_valid - preds_valid))
    }

    overall_metrics[[target_var]] <- list(
        Rsquared = rsq_val,
        RMSE = rmse_val,
        MAE = mae_val
    )
}

# Format results into a data frame
results_summary_xgb <- data.frame(
    Target = names(overall_metrics),
    Overall_Rsquared = sapply(overall_metrics, `[[`, "Rsquared"),
    Overall_RMSE = sapply(overall_metrics, `[[`, "RMSE"),
    Overall_MAE = sapply(overall_metrics, `[[`, "MAE")
)
rownames(results_summary_xgb) <- NULL

print("Overall Performance Metrics (XGBoost - across all folds):")
print(round(results_summary_xgb, 4))

# Optional: Inspect best hyperparameters found per fold
# print("Best hyperparameters found per fold/target:")
# print(best_hyperparams)

message("--- Script Finished ---") 