# predict_glasgow.R

# --- 1. Load Libraries ---
# Ensure required packages are installed
# install.packages(c("glmnet", "caret", "dplyr", "Metrics")) # Metrics is optional for performance
library(glmnet)
library(caret)
library(dplyr)
# library(Metrics) # Optional: for rmse/rsq functions

message("Libraries loaded.")

# --- 2. Load Data ---
# Load the wordfeatures package environment to access data and functions
if (requireNamespace("devtools", quietly = TRUE) && interactive()) {
  message("Loading package 'wordfeatures' using devtools::load_all()")
  devtools::load_all()
} else if (requireNamespace("wordfeatures", quietly = TRUE)) {
   message("Loading package 'wordfeatures' using library()")
   library(wordfeatures)
} else {
  stop("Could not load the 'wordfeatures' package. Please install or load it.")
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

# Define target variables (mean ratings)
target_vars <- c("arousal_m", "valence_m", "dominance_m", "concreteness_m",
                 "imageability_m", "familiarity_m", "aoa_m", "gender_m", "size_m") # Added size_m based on earlier script

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
# Check consistency of embedding dimensions
emb_dim <- unique(sapply(glasgow_embeddings, length))
if (length(emb_dim) > 1) {
    stop("Embeddings have inconsistent dimensions.")
}
if (emb_dim[1] == 0) {
    stop("Embeddings have zero dimensions.")
}
embedding_matrix <- do.call(rbind, glasgow_embeddings)
rownames(embedding_matrix) <- words_in_embeddings
message("Embedding matrix created with dimensions: ", nrow(embedding_matrix), " x ", ncol(embedding_matrix))

# Align norms and embeddings
common_words <- intersect(glasgow_norms$word, words_in_embeddings)
if (length(common_words) == 0) {
    stop("No common words found between glasgow_norms and glasgow_embeddings.")
}

glasgow_norms_aligned <- glasgow_norms %>% 
  filter(word %in% common_words) %>% 
  arrange(word) # Sort by word

embedding_matrix_aligned <- embedding_matrix[glasgow_norms_aligned$word, ] # Order rows by sorted words

message("Aligned norms and embeddings. Number of observations: ", length(common_words))

# Select target columns and handle potential NAs in targets
Y_full <- glasgow_norms_aligned %>% select(all_of(target_vars))

# Remove rows where *any* target variable is NA
complete_target_rows <- complete.cases(Y_full)
if(sum(!complete_target_rows) > 0) {
    message("Removing ", sum(!complete_target_rows), " rows with NA values in target variables.")
}
Y <- as.matrix(Y_full[complete_target_rows, ])
X <- embedding_matrix_aligned[complete_target_rows, ]
aligned_words <- glasgow_norms_aligned$word[complete_target_rows] # Keep track of words used

if(nrow(Y) < 20) { # Arbitrary small number check
    stop("Too few complete observations remaining after NA removal.")
}

message("Final data prepared for glmnet. X dimensions: ", nrow(X), " x ", ncol(X), ", Y dimensions: ", nrow(Y), " x ", ncol(Y))

# --- 4. Set up Cross-Validation ---
k_folds <- 10
set.seed(123) # for reproducibility
cv_folds <- caret::createFolds(1:nrow(Y), k = k_folds, list = TRUE, returnTrain = FALSE) # Get test indices per fold

message(paste0("Created ", k_folds, "-fold cross-validation indices."))

# --- 5. Initialize Results Storage ---
all_preds <- list()
all_actuals <- list()
all_lambdas <- numeric(k_folds)
# Optional: Store metrics per fold if desired
fold_rsq <- matrix(NA, nrow = k_folds, ncol = length(target_vars), dimnames = list(paste0("Fold", 1:k_folds), target_vars))

# --- 6. Cross-Validation Loop ---
alpha_val <- 0.5 # Elastic Net parameter

for (i in 1:k_folds) {
  message(paste("Processing Fold", i, "of", k_folds, "..."))

  # Get indices for training and testing sets
  test_indices <- cv_folds[[i]]
  train_indices <- setdiff(1:nrow(Y), test_indices)

  X_train <- X[train_indices, ]
  Y_train <- Y[train_indices, ]
  X_test <- X[test_indices, ]
  Y_test <- Y[test_indices, ]

  # Inner CV for lambda selection on the training data
  # Use tryCatch for potential convergence issues with cv.glmnet
  cv_fit <- tryCatch({
      cv.glmnet(X_train, Y_train, family = "mgaussian", alpha = alpha_val, nfolds = 10) # Default nfolds=10 is fine
  }, error = function(e) {
      warning("cv.glmnet failed for fold ", i, ": ", e$message, ". Skipping fold.")
      return(NULL)
  })

  if (is.null(cv_fit)) {
      all_lambdas[i] <- NA
      next # Skip to next fold
  }

  best_lambda <- cv_fit$lambda.min
  all_lambdas[i] <- best_lambda
  message(paste("  Best lambda found (lambda.min):", format(best_lambda, digits=4)))

  # Train final model for this fold using the best lambda
  # Use tryCatch for potential convergence issues with glmnet
   final_model <- tryCatch({
       glmnet(X_train, Y_train, family = "mgaussian", alpha = alpha_val, lambda = best_lambda)
   }, error = function(e) {
       warning("glmnet training failed for fold ", i, " with lambda ", best_lambda, ": ", e$message, ". Skipping predictions for fold.")
       return(NULL)
   })

   if (is.null(final_model)) {
       next # Skip to next fold
   }

  # Predict on the test set
  # predict.glmnet for mgaussian with a single lambda returns a 3D array [obs, response, 1]
  predictions <- tryCatch({
      # Extract the 2D matrix [obs, response] by selecting the first slice
      predict(final_model, newx = X_test, s = best_lambda, type = "response")[,,1]
  }, error = function(e) {
      warning("Prediction failed for fold ", i, ": ", e$message, ". Skipping.")
      return(NULL)
  })

  if (is.null(predictions)) {
       next # Skip to next fold
   }

  # Store results (ensure consistent naming/structure)
  colnames(predictions) <- colnames(Y_test) # Ensure prediction columns are named
  all_preds[[i]] <- as.data.frame(predictions)
  all_actuals[[i]] <- as.data.frame(Y_test)

  # Calculate R-squared per target for this fold
  for (j in 1:ncol(Y_test)) {
      # Check for sufficient variance before calculating correlation
      if (var(Y_test[, j], na.rm = TRUE) > 1e-8 && var(predictions[, j], na.rm = TRUE) > 1e-8) {
          fold_rsq[i, j] <- cor(Y_test[, j], predictions[, j], use="pairwise.complete.obs")^2
      } else {
          fold_rsq[i, j] <- NA # Assign NA if variance is too low
      }
  }
   message(paste("  Fold", i, "completed."))
}

# --- 7. Aggregate and Report Results ---
message("\n--- Cross-Validation Results ---")

# Combine predictions and actuals from all folds
# Note: The order will correspond to the order of test sets in cv_folds
if (length(all_preds) > 0 && length(all_preds) == length(all_actuals)) {
    combined_preds <- do.call(rbind, all_preds)
    combined_actuals <- do.call(rbind, all_actuals)

    # Calculate overall R-squared, RMSE, and MAE for each target variable
    overall_rsq <- numeric(length(target_vars))
    overall_rmse <- numeric(length(target_vars))
    overall_mae <- numeric(length(target_vars))
    names(overall_rsq) <- names(overall_rmse) <- names(overall_mae) <- target_vars

    for (j in 1:ncol(combined_actuals)) {
        var_name <- colnames(combined_actuals)[j]
        actual_vals <- combined_actuals[, j]
        pred_vals <- combined_preds[, j]

        # Calculate metrics only if variance is sufficient and no NAs in relevant pair
        valid_comparison <- !is.na(actual_vals) & !is.na(pred_vals)
        actual_vals_valid <- actual_vals[valid_comparison]
        pred_vals_valid <- pred_vals[valid_comparison]
        
        if(length(actual_vals_valid) > 1 && # Need at least 2 points for variance/cor
           var(actual_vals_valid) > 1e-8 && 
           var(pred_vals_valid) > 1e-8) {
            
            overall_rsq[var_name] <- cor(actual_vals_valid, pred_vals_valid)^2
            overall_rmse[var_name] <- sqrt(mean((actual_vals_valid - pred_vals_valid)^2))
            overall_mae[var_name] <- mean(abs(actual_vals_valid - pred_vals_valid))
        } else {
            overall_rsq[var_name] <- NA
            overall_rmse[var_name] <- NA
            overall_mae[var_name] <- NA
        }
    }

    # Combine into a results data frame
    results_summary <- data.frame(
        Target = target_vars,
        Overall_Rsquared = overall_rsq,
        Overall_RMSE = overall_rmse,
        Overall_MAE = overall_mae,
        Avg_Fold_Rsquared = colMeans(fold_rsq, na.rm = TRUE)
    )
    rownames(results_summary) <- NULL # Remove row names for cleaner printing

    print("Overall Performance Metrics (across all folds):")
    print(results_summary)

    # Remove individual print statements for R2 and Avg Fold R2 as they are in the table
    # print("Overall R-squared (across all folds):")
    # print(round(overall_rsq, 4))
    # print("Average Fold R-squared:")
    # print(round(colMeans(fold_rsq, na.rm = TRUE), 4))

} else {
    message("No successful predictions were made across the folds.")
}

print("Lambdas selected (lambda.min) per fold:")
print(round(all_lambdas, 4))

# Optional: Save detailed results
# results_df <- cbind(Word = aligned_words[unlist(cv_folds)], combined_actuals, combined_preds)
# write.csv(results_df, "predict_glasgow_results.csv", row.names = FALSE)

message("--- Script Finished ---") 