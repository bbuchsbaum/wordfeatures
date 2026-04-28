# data-raw/mrc_psycholinguistic_database.R
#
# Build a cleaned version of the classic MRC Psycholinguistic Database
# (Coltheart and Wilson, version 2.00, OTA item 1054).

mrc_url <- "https://llds.ling-phil.ox.ac.uk/llds/xmlui/bitstream/handle/20.500.14106/1054/1054.zip"
local_archive <- file.path("data-raw", "mrc_psycholinguistic_database_ota1054.zip")

normalize_word <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("\\s+", " ", x)
  x
}

zero_to_na <- function(x) {
  x[x == 0] <- NA_real_
  x
}

archive_path <- if (file.exists(local_archive)) {
  local_archive
} else {
  file.path(tempdir(), basename(mrc_url))
}

if (!file.exists(archive_path)) {
  download.file(mrc_url, destfile = archive_path, mode = "wb")
}

lines <- readLines(unz(archive_path, "mrc2.dct"), warn = FALSE, encoding = "latin1")

fixed_block <- substr(lines, 1, 51)
variable_block <- substr(lines, 52, nchar(lines))
variable_parts <- strsplit(variable_block, "|", fixed = TRUE)

get_part <- function(parts, idx) {
  vapply(parts, function(x) {
    if (length(x) >= idx) trimws(x[[idx]]) else ""
  }, character(1))
}

parse_num_field <- function(start, end) {
  suppressWarnings(as.numeric(substr(fixed_block, start, end)))
}

mrc_psycholinguistic_norms <- data.frame(
  word = normalize_word(get_part(variable_parts, 1)),
  word_original = get_part(variable_parts, 1),
  phonetic = get_part(variable_parts, 2),
  phonetic_edited = get_part(variable_parts, 3),
  stress_pattern = get_part(variable_parts, 4),
  n_letters = parse_num_field(1, 2),
  n_phonemes = parse_num_field(3, 4),
  n_syllables = parse_num_field(5, 5),
  kf_frequency = parse_num_field(6, 10),
  kf_n_categories = parse_num_field(11, 12),
  kf_n_samples = parse_num_field(13, 15),
  tl_frequency = parse_num_field(16, 21),
  brown_frequency = parse_num_field(22, 25),
  familiarity = parse_num_field(26, 28),
  concreteness = parse_num_field(29, 31),
  imageability = parse_num_field(32, 34),
  meaningfulness_colorado = parse_num_field(35, 37),
  meaningfulness_pavio = parse_num_field(38, 40),
  age_of_acquisition = parse_num_field(41, 43),
  tq2 = trimws(substr(fixed_block, 44, 44)),
  word_type = trimws(substr(fixed_block, 45, 45)),
  pd_word_type = trimws(substr(fixed_block, 46, 46)),
  alphasyllable = trimws(substr(fixed_block, 47, 47)),
  status = trimws(substr(fixed_block, 48, 48)),
  variant_phoneme = trimws(substr(fixed_block, 49, 49)),
  written_capitalized = trimws(substr(fixed_block, 50, 50)),
  irregular_plural = trimws(substr(fixed_block, 51, 51)),
  stringsAsFactors = FALSE
)

mrc_psycholinguistic_norms$n_phonemes <- zero_to_na(mrc_psycholinguistic_norms$n_phonemes)
mrc_psycholinguistic_norms$n_syllables <- zero_to_na(mrc_psycholinguistic_norms$n_syllables)
mrc_psycholinguistic_norms$kf_frequency <- zero_to_na(mrc_psycholinguistic_norms$kf_frequency)
mrc_psycholinguistic_norms$kf_n_categories <- zero_to_na(mrc_psycholinguistic_norms$kf_n_categories)
mrc_psycholinguistic_norms$kf_n_samples <- zero_to_na(mrc_psycholinguistic_norms$kf_n_samples)
mrc_psycholinguistic_norms$tl_frequency <- zero_to_na(mrc_psycholinguistic_norms$tl_frequency)
mrc_psycholinguistic_norms$brown_frequency <- zero_to_na(mrc_psycholinguistic_norms$brown_frequency)
mrc_psycholinguistic_norms$familiarity <- zero_to_na(mrc_psycholinguistic_norms$familiarity)
mrc_psycholinguistic_norms$concreteness <- zero_to_na(mrc_psycholinguistic_norms$concreteness)
mrc_psycholinguistic_norms$imageability <- zero_to_na(mrc_psycholinguistic_norms$imageability)
mrc_psycholinguistic_norms$meaningfulness_colorado <- zero_to_na(mrc_psycholinguistic_norms$meaningfulness_colorado)
mrc_psycholinguistic_norms$meaningfulness_pavio <- zero_to_na(mrc_psycholinguistic_norms$meaningfulness_pavio)
mrc_psycholinguistic_norms$age_of_acquisition <- zero_to_na(mrc_psycholinguistic_norms$age_of_acquisition)

mrc_psycholinguistic_norms <- mrc_psycholinguistic_norms[
  nzchar(mrc_psycholinguistic_norms$word),
]

mrc_psycholinguistic_norms <- mrc_psycholinguistic_norms[order(
  mrc_psycholinguistic_norms$word,
  mrc_psycholinguistic_norms$word_original
), ]
row.names(mrc_psycholinguistic_norms) <- NULL

save(
  mrc_psycholinguistic_norms,
  file = file.path("data", "mrc_psycholinguistic_norms.rda"),
  compress = "bzip2"
)

message(
  "Saved mrc_psycholinguistic_norms (", nrow(mrc_psycholinguistic_norms),
  " rows; ", sum(!is.na(mrc_psycholinguistic_norms$imageability)),
  " with imageability, ", sum(!is.na(mrc_psycholinguistic_norms$concreteness)),
  " with concreteness)."
)
