# data-raw/norare_english_norms.R
#
# Build additional English lexical norm resources from the NoRaRe CLDF release.
# The selected variables expand imageability coverage and add semantically useful
# predictors for imageability modeling.

norare_url <- "https://zenodo.org/records/14925245/files/concepticon/norare-cldf-v1.1.zip"
local_archive <- file.path("data-raw", "norare-cldf-v1.1.zip")

normalize_word <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("\\s+", " ", x)
  x
}

archive_path <- if (file.exists(local_archive)) {
  local_archive
} else {
  file.path(tempdir(), basename(norare_url))
}

if (!file.exists(archive_path)) {
  download.file(norare_url, destfile = archive_path, mode = "wb")
}

extract_dir <- tempfile("norare_cldf_")
dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
utils::unzip(archive_path, exdir = extract_dir)

cldf_root <- list.files(extract_dir,
                        pattern = "^concepticon-norare-cldf-",
                        full.names = TRUE)

if (length(cldf_root) != 1) {
  stop("Could not identify extracted NoRaRe CLDF directory.")
}

cldf_dir <- file.path(cldf_root[[1]], "cldf")
norare_inner_zip <- file.path(cldf_dir, "norare.csv.zip")

datasets <- read.csv(file.path(cldf_dir, "datasets.csv"),
                     stringsAsFactors = FALSE,
                     colClasses = "character")
variables <- read.csv(file.path(cldf_dir, "variables.csv"),
                      stringsAsFactors = FALSE,
                      colClasses = "character")
glosses <- read.csv(file.path(cldf_dir, "glosses.csv"),
                    stringsAsFactors = FALSE,
                    colClasses = "character")
norare_values <- read.csv(unz(norare_inner_zip, "norare.csv"),
                          stringsAsFactors = FALSE,
                          colClasses = "character")

selected_features <- data.frame(
  variable_id = c(
    "Clark-2004-ImageryFamiliarity-ENGLISH_IMAGEABILITY_MEAN",
    "Clark-2004-ImageryFamiliarity-ENGLISH_FAMILIARITY_MEAN",
    "Gilhooly-1980-Ratings-ENGLISH_IMAGEABILITY_MEAN",
    "Gilhooly-1980-Ratings-ENGLISH_FAMILIARITY_MEAN",
    "Gilhooly-1980-Ratings-ENGLISH_CONCRETENESS_MEAN",
    "Gilhooly-1980-Ratings-ENGLISH_AOA_MEAN",
    "Scott-2019-Ratings-ENGLISH_IMAGEABILITY_MEAN",
    "Scott-2019-Ratings-ENGLISH_FAMILIARITY_MEAN",
    "Scott-2019-Ratings-ENGLISH_CONCRETENESS_MEAN",
    "Scott-2019-Ratings-ENGLISH_AOA_MEAN",
    "Scott-2019-Ratings-ENGLISH_SEM_SIZE_MEAN",
    "Scott-2019-Ratings-ENGLISH_VALENCE_MEAN",
    "Scott-2019-Ratings-ENGLISH_AROUSAL_MEAN",
    "Scott-2019-Ratings-ENGLISH_DOMINANCE_MEAN",
    "Brysbaert-2014-Concreteness-ENGLISH_CONCRETENESS_MEAN",
    "Juhasz-2013-SER-ENGLISH_SER_MEAN",
    "Warriner-2013-AffectiveRatings-ENGLISH_VALENCE_MEAN",
    "Warriner-2013-AffectiveRatings-ENGLISH_AROUSAL_MEAN",
    "Warriner-2013-AffectiveRatings-ENGLISH_DOMINANCE_MEAN",
    "Pexman-2019-Sensorimotor-ENGLISH_BOI_MEAN",
    "Winter-2024-Iconicity-ENGLISH_ICONICITY_MEAN"
  ),
  feature_name = c(
    "imageability_clark_2004",
    "familiarity_clark_2004",
    "imageability_gilhooly_logie_1980",
    "familiarity_gilhooly_logie_1980",
    "concreteness_gilhooly_logie_1980",
    "aoa_gilhooly_logie_1980",
    "imageability_glasgow_2019",
    "familiarity_glasgow_2019",
    "concreteness_glasgow_2019",
    "aoa_glasgow_2019",
    "semantic_size_glasgow_2019",
    "valence_glasgow_2019",
    "arousal_glasgow_2019",
    "dominance_glasgow_2019",
    "concreteness_brysbaert_2014",
    "sensory_experience_juhasz_2013",
    "valence_warriner_2013",
    "arousal_warriner_2013",
    "dominance_warriner_2013",
    "boi_pexman_2019",
    "iconicity_winter_2024"
  ),
  stringsAsFactors = FALSE
)

variable_subset <- merge(selected_features,
                         variables,
                         by.x = "variable_id",
                         by.y = "ID",
                         all.x = TRUE,
                         sort = FALSE)

if (any(is.na(variable_subset$Dataset_ID))) {
  missing_ids <- variable_subset$variable_id[is.na(variable_subset$Dataset_ID)]
  stop("Missing NoRaRe variable definitions: ", paste(missing_ids, collapse = ", "))
}

value_subset <- norare_values[norare_values$Variable_ID %in% selected_features$variable_id, ]
gloss_subset <- glosses[glosses$ID %in% value_subset$Unit_ID, ]
dataset_subset <- datasets[datasets$ID %in% unique(variable_subset$Dataset_ID), ]

norms_long <- merge(value_subset,
                    gloss_subset[, c("ID", "Form", "Dataset_ID", "Parameter_ID")],
                    by.x = "Unit_ID",
                    by.y = "ID",
                    all.x = TRUE,
                    sort = FALSE)
norms_long <- merge(norms_long,
                    variable_subset[, c("variable_id", "feature_name", "Dataset_ID",
                                        "Name", "Result")],
                    by.x = "Variable_ID",
                    by.y = "variable_id",
                    all.x = TRUE,
                    sort = FALSE)
norms_long <- merge(norms_long,
                    dataset_subset[, c("ID", "Name", "Year", "URL", "Citation")],
                    by.x = "Dataset_ID.x",
                    by.y = "ID",
                    all.x = TRUE,
                    sort = FALSE,
                    suffixes = c("_variable", "_dataset"))

norms_long$value <- suppressWarnings(as.numeric(norms_long$Value))
norms_long$word <- normalize_word(norms_long$Form)

norms_long <- norms_long[!is.na(norms_long$value) &
                           !is.na(norms_long$word) &
                           nzchar(norms_long$word), ]

norare_english_norms <- norms_long[, c(
  "word",
  "Form",
  "Parameter_ID",
  "Dataset_ID.x",
  "Name_dataset",
  "Year",
  "URL",
  "Citation",
  "Variable_ID",
  "feature_name",
  "Result",
  "value"
)]

names(norare_english_norms) <- c(
  "word",
  "form",
  "concept_id",
  "dataset_id",
  "dataset_name",
  "year",
  "source_url",
  "citation",
  "variable_id",
  "feature_name",
  "measurement",
  "value"
)

norare_english_norms <- norare_english_norms[order(
  norare_english_norms$word,
  norare_english_norms$feature_name
), ]
row.names(norare_english_norms) <- NULL

feature_values <- aggregate(
  value ~ word + feature_name,
  data = norare_english_norms,
  FUN = function(x) mean(x, na.rm = TRUE)
)

imageability_model_features <- reshape(
  feature_values,
  idvar = "word",
  timevar = "feature_name",
  direction = "wide"
)

names(imageability_model_features) <- sub("^value\\.", "",
                                          names(imageability_model_features))
imageability_model_features <- imageability_model_features[order(
  imageability_model_features$word
), ]
row.names(imageability_model_features) <- NULL

save(norare_english_norms,
     file = file.path("data", "norare_english_norms.rda"),
     compress = "bzip2")
save(imageability_model_features,
     file = file.path("data", "imageability_model_features.rda"),
     compress = "bzip2")

message(
  "Saved norare_english_norms (", nrow(norare_english_norms),
  " rows) and imageability_model_features (",
  nrow(imageability_model_features), " words)."
)
