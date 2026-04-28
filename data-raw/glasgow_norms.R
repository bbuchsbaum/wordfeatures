# data-raw/glasgow_norms.R

# Load necessary libraries
# Ensure dplyr and stringr are installed: install.packages(c("dplyr", "stringr"))
library(dplyr)
library(stringr)

# Load the raw CSV data, using the first row as header (R will fill blanks)
# Handle potential empty strings or standard NA representations as NA
glasgow_norms_raw <- read.csv("data-raw/13428_2018_1099_MOESM2_ESM.csv",
                              stringsAsFactors = FALSE,
                              header = TRUE, # Use the first row as header
                              # skip = 1, # REMOVED: Do not skip the first row
                              na.strings = c("NA", ""))

# Define the renaming scheme based on inferred meanings
# R's default for blank headers in line 1 should generate X, X.1, etc.
# Original Name = New Name
new_names_map <- c(
  Words = "word", Length = "length",
  AROU = "arousal_m", X = "arousal_sd", X.1 = "arousal_n",
  VAL = "valence_m", X.2 = "valence_sd", X.3 = "valence_n",
  DOM = "dominance_m", X.4 = "dominance_sd", X.5 = "dominance_n",
  CNC = "concreteness_m", X.6 = "concreteness_sd", X.7 = "concreteness_n",
  IMAG = "imageability_m", X.8 = "imageability_sd", X.9 = "imageability_n",
  FAM = "familiarity_m", X.10 = "familiarity_sd", X.11 = "familiarity_n",
  AOA = "aoa_m", X.12 = "aoa_sd", X.13 = "aoa_n",
  SIZE = "size_m", X.14 = "size_sd", X.15 = "size_n",
  GEND = "gender_m", X.16 = "gender_sd", X.17 = "gender_n"
)

# Check if all expected old names exist in the raw data
missing_cols <- setdiff(names(new_names_map), colnames(glasgow_norms_raw))
if (length(missing_cols) > 0) {
  warning("The following expected columns were not found in the CSV: ", paste(missing_cols, collapse=", "))
}

# --- Corrected Renaming --- 
# Invert the map so it's new_name = old_name for dplyr::rename()
# Suppressing warnings in case of duplicate new names (shouldn't happen here)
inverted_names_map <- suppressWarnings(setNames(names(new_names_map), new_names_map))

# Filter the map to include only the NEW names whose corresponding OLD names exist in the raw data
existing_old_names <- intersect(names(new_names_map), colnames(glasgow_norms_raw))
# Corrected filter: Check if the VALUES (old names) of the inverted map are present
map_to_use <- inverted_names_map[inverted_names_map %in% existing_old_names]

# Rename using the filtered inverted map
glasgow_processed <- glasgow_norms_raw %>% 
  dplyr::rename(!!!map_to_use)
# --- End Corrected Renaming ---

# Perform context extraction and word cleaning in a separate step
if ("word" %in% colnames(glasgow_processed)) {
  glasgow_processed <- glasgow_processed %>%
    dplyr::mutate(
      # Extract content within parentheses, handling potential missing values
      context = ifelse(stringr::str_detect(word, "\\(([^)]+)\\)"), stringr::str_extract(word, "(?<=\\()([^)]+)(?=\\))"), NA_character_),
      # Remove context part (including parentheses and surrounding spaces) from word
      word = stringr::str_remove(word, "\\s*\\([^)]+\\)\\s*"),
      # Trim leading/trailing whitespace from word and context
      word = stringr::str_trim(word),
      context = stringr::str_trim(context)
    ) %>%
    # Move context column after word column
    dplyr::relocate(context, .after = word)
} else {
  warning("Column 'word' (originally 'Words') not found after renaming. Skipping context extraction.")
}

# Convert relevant columns to numeric, suppressing warnings for coercion issues
# Identify columns that should be numeric (all derived from new_names_map except 'word')
numeric_cols <- setdiff(new_names_map[existing_old_names], "word")
# Filter to only those present in the processed data frame
numeric_cols_present <- intersect(numeric_cols, colnames(glasgow_processed))

glasgow_processed <- glasgow_processed %>% 
  mutate(across(all_of(numeric_cols_present), ~ suppressWarnings(as.numeric(as.character(.))))) 


# Assign to the final object name
glasgow_norms <- glasgow_processed

# Save the final data object to data/glasgow_norms.rda
# This makes it available within the package after installation.
usethis::use_data(glasgow_norms, overwrite = TRUE)

message("Glasgow norms processed: context extracted, columns renamed and saved to data/glasgow_norms.rda") 