# wordfeatures

Lexical norm datasets for English words, plus utilities to fetch word
embeddings from OpenAI / Google and predict imageability from a bundled
regularized regression model.

## Install

```r
# install.packages("pak")
pak::pak("path/to/wordfeatures")          # local source
# pak::pak("bbuchsbaum/wordfeatures")     # from GitHub
```

R >= 4.1 is required.

## What you get without any API key

The package ships pre-computed data — these are immediately usable after
`library(wordfeatures)` with **no API call and no API key**:

| Dataset                       | Description                                                          |
|-------------------------------|----------------------------------------------------------------------|
| `glasgow_norms`               | Glasgow Norms (imageability, concreteness, AoA, valence, ...)        |
| `sensorimotor_norms`          | Lancaster Sensorimotor Norms (visual, auditory, haptic, ...)         |
| `mrc_psycholinguistic_norms`  | MRC Psycholinguistic Database (Coltheart and Wilson)                 |
| `norare_english_norms`        | Long-format English norms aggregated from NoRaRe                     |
| `imageability_model_features` | Wide lexical predictor table built from NoRaRe                       |
| `imageability_training_frame` | Merged modeling frame used to fit the bundled model                  |
| `glasgow_embeddings`          | Pre-computed OpenAI `text-embedding-3-large` vectors for Glasgow words |

```r
library(wordfeatures)
head(glasgow_norms)
glasgow_embeddings[["cat"]]   # 3072-dim numeric vector, no API call
```

## API keys: when you need them, what they accept

The package has **two** exported functions that talk to embedding APIs.
Both require an API key. Nothing else in the package does.

| Function                  | Needs an API key?                                |
|---------------------------|--------------------------------------------------|
| `word_embeddings()`       | **Yes**, for the chosen provider                 |
| `predict_imageability()`  | **Yes** — it calls `word_embeddings()` internally |
| Anything else (data sets) | No                                               |

### Where the key is read from

`word_embeddings()` and `predict_imageability()` look up keys from
environment variables by default. You can override per call via function
arguments.

| Provider | Env var (default)              | Fallback env var | Function argument   |
|----------|--------------------------------|------------------|---------------------|
| OpenAI   | `OPENAI_API_KEY`               | —                | `api_key = "..."`   |
| Google   | `GOOGLE_API_KEY`               | `GEMINI_API_KEY` | `google_api_key = "..."` |

Set keys in your R session, in `~/.Renviron`, or in your shell:

```r
Sys.setenv(OPENAI_API_KEY = "sk-...")
Sys.setenv(GOOGLE_API_KEY = "AIza...")        # or GEMINI_API_KEY
```

### What kinds of keys are accepted

- **OpenAI** — a standard OpenAI API key (typically starting with `sk-`)
  with access to the Embeddings endpoint
  (`https://api.openai.com/v1/embeddings`). Project keys, user keys, and
  service-account keys all work as long as they have embeddings scope.
- **Google** — a Google AI Studio / Generative Language API key (typically
  starting with `AIza`) with access to
  `generativelanguage.googleapis.com`. Vertex AI service-account
  credentials are **not** accepted; use an AI Studio key. The same key is
  used regardless of whether you set it via `GOOGLE_API_KEY` or
  `GEMINI_API_KEY`.

If a key is missing or invalid, both functions raise an informative error
telling you which env var to set.

## Quick start

### Get embeddings (`word_embeddings`)

```r
library(wordfeatures)

# OpenAI (default provider). Requires OPENAI_API_KEY.
embs <- word_embeddings(
  c("cat", "dog", "abstraction"),
  provider = "openai",
  model    = "text-embedding-3-small"   # default; or "text-embedding-3-large"
)
length(embs)         # 3
length(embs[["cat"]]) # 1536 for -3-small, 3072 for -3-large

# Google (requires GOOGLE_API_KEY or GEMINI_API_KEY).
embs_g <- word_embeddings(
  c("cat", "dog"),
  provider     = "google",
  google_model = "embedding-001"
)
```

Returns a named list of numeric vectors (one per input). Names mirror the
input order, including duplicates. Returns `NULL` only when `texts` is
empty.

### Predict imageability (`predict_imageability`)

```r
predict_imageability(c("cat", "dog", "theoretical"))
# Default: OpenAI text-embedding-3-large (matches the bundled model)
```

The bundled glmnet model was trained on **OpenAI `text-embedding-3-large`**
embeddings combined with NoRaRe-derived lexical features. Calling
`predict_imageability()` with a different provider or model will still
work, but the function emits a `warning()` about possible degraded
predictions because the embedding space no longer matches the model.

```r
# Same call, but force Google embeddings — will warn.
predict_imageability(
  c("cat", "dog"),
  embedding_provider = "google",
  embedding_model    = "gemini-embedding-001"
)
```

Words missing from the bundled lexical lookup table get zero-filled
features plus a missingness flag, so the model handles unseen vocabulary
gracefully.

## Imageability model details

- **Target:** mean Glasgow imageability per normalized word
- **Embeddings:** OpenAI `text-embedding-3-large` (3072 dims)
- **Lexical predictors:** non-leaky NoRaRe-derived ratings (concreteness,
  AoA, valence, arousal, sensory experience, body-object interaction,
  iconicity, ...)
- **Missingness handling:** every lexical predictor is paired with a
  `<feature>_missing` indicator column
- **Model:** glmnet (regularized linear regression), bundled internally

## API costs and rate limits

Embedding APIs are pay-per-token. `word_embeddings()` batches all inputs
into a single request per call, so cost scales roughly with the total
number of input tokens. The bundled retry policy automatically retries on
HTTP 429/500/502/503/504 up to 3 times. For large jobs, prefer
`text-embedding-3-small` (cheaper, 1536 dims) unless you specifically need
the 3072-dim space the imageability model was trained in.

## Common commands

```bash
R -q -e "devtools::document()"   # rebuild NAMESPACE and man/ from roxygen
R -q -e "devtools::test()"       # run tests
R CMD build .                    # build source tarball
```

## License

GPL-3
