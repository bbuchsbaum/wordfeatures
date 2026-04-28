# data-raw/sensorimotor_norms.R

# Load required libraries
# Ensure missMDA is installed: install.packages("missMDA")
library(missMDA)

# Load the raw CSV data
sensorimotor_norms_raw <- read.csv("data-raw/Sensorimotor_norms_09Apr2025.csv", stringsAsFactors = FALSE)

# Check for missing values
if (any(is.na(sensorimotor_norms_raw))) {
  message("Missing values found. Imputing using missMDA::imputeFAMD...")

  # Convert character columns to factors for FAMD
  char_cols <- sapply(sensorimotor_norms_raw, is.character)
  if (any(char_cols)) {
    sensorimotor_norms_raw[char_cols] <- lapply(sensorimotor_norms_raw[char_cols], factor)
  }

  # Estimate the number of dimensions for imputation
  # Reducing ncp.max and nbsim for speed in this example; adjust as needed.
  # Using tryCatch in case estimation fails for simpler datasets
  ncp_est <- tryCatch({
      missMDA::estim_ncpFAMD(sensorimotor_norms_raw, ncp.max = 5, nbsim = 20, verbose = FALSE)$ncp
    },
    error = function(e) {
      warning("estim_ncpFAMD failed, using default ncp=2. Error: ", e$message)
      return(2) # Default to 2 dimensions if estimation fails
    }
  )
  message(paste("Estimated optimal number of dimensions (ncp) for FAMD imputation:", ncp_est))

  # Perform imputation using FAMD
  # Using suppressMessages to hide routine missMDA output
  imputed_data <- suppressMessages(missMDA::imputeFAMD(sensorimotor_norms_raw, ncp = ncp_est))

  # Assign the imputed data to the final object name
  sensorimotor_norms <- imputed_data$completeObs
  message("Imputation complete.")

} else {
  message("No missing values found. Skipping imputation.")
  # Assign to the final object name directly if no NAs
  sensorimotor_norms <- sensorimotor_norms_raw
}

# Optional: Add any further cleaning or processing steps here

# Save the final data object to data/sensorimotor_norms.rda
# This makes it available within the package after installation.
usethis::use_data(sensorimotor_norms, overwrite = TRUE) 