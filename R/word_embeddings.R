#' Get Word Embeddings from OpenAI or Google AI APIs
#'
#' This function takes a character vector or list of words/phrases
#' and retrieves their embeddings from the specified provider.
#'
#' @param texts A character vector or list of strings (words or phrases) to embed.
#' @param model A string specifying the OpenAI embedding model to use
#'        (e.g., "text-embedding-3-small", "text-embedding-3-large",
#'        "text-embedding-ada-002"). Defaults to "text-embedding-3-small".
#' @param api_key A string containing your OpenAI API key. Defaults to the value
#'        of the environment variable `OPENAI_API_KEY`.
#' @param api_url A string specifying the OpenAI API endpoint URL for embeddings.
#'        Defaults to "https://api.openai.com/v1/embeddings".
#' @param provider A string specifying the API provider ("openai" or "google").
#'        Defaults to "openai".
#' @param google_api_key A string containing your Google AI API key. Defaults to the
#'        value of `GOOGLE_API_KEY`, falling back to `GEMINI_API_KEY` when unset.
#' @param google_model A string specifying the Google embedding model (e.g., "embedding-001").
#'        Defaults to "embedding-001".
#' @param google_api_url_base A string specifying the base Google API endpoint URL.
#'        Defaults to "https://generativelanguage.googleapis.com/v1beta/models/".
#' @param ... Additional arguments passed to `httr2::req_perform()`, e.g., `verbose = TRUE`.
#'
#' @return A named list where names are the input texts and values are numeric
#'   vectors representing the embeddings. If the input `texts` contain duplicates,
#'   the names in the returned list will also contain duplicates. Returns `NULL`
#'   only when `texts` is empty.
#' @export
#'
#' @examples
#' \dontrun{
#' # Ensure your API key is set as an environment variable
#' # Sys.setenv(OPENAI_API_KEY = "your-key-here")
#'
#' words_to_embed <- c("apple", "banana", "fruit", "king", "queen", "monarchy")
#' embeddings <- word_embeddings(words_to_embed)
#'
#' if (!is.null(embeddings)) {
#'   print(paste("Got embeddings for", length(embeddings), "items."))
#'   print(names(embeddings))
#'   # Access embedding for "apple"
#'   print(head(embeddings[["apple"]], 5))
#'   print(paste("Dimension of 'apple':", length(embeddings[["apple"]])))
#' }
#'
#' phrases_to_embed <- list("red car", "fast blue car")
#' phrase_embeddings <- word_embeddings(phrases_to_embed, model="text-embedding-3-large")
#' if (!is.null(phrase_embeddings)) {
#'  str(phrase_embeddings)
#' }
#'
#' # --- OpenAI Example ---
#' # Sys.setenv(OPENAI_API_KEY = "your-openai-key-here")
#' words_oai <- c("apple", "banana", "fruit")
#' embeddings_oai <- word_embeddings(words_oai, provider = "openai")
#' str(embeddings_oai)
#'
#' # --- Google Example ---
#' # Sys.setenv(GOOGLE_API_KEY = "your-google-key-here")
#' words_goog <- c("car", "truck", "vehicle")
#' embeddings_goog <- word_embeddings(words_goog, provider = "google")
#' str(embeddings_goog)
#' }
word_embeddings <- function(texts,
                            model = "text-embedding-3-small",
                            api_key = Sys.getenv("OPENAI_API_KEY"),
                            api_url = "https://api.openai.com/v1/embeddings",
                            provider = c("openai", "google"),
                            google_api_key = {
                              key <- Sys.getenv("GOOGLE_API_KEY")
                              if (!nzchar(key)) {
                                key <- Sys.getenv("GEMINI_API_KEY")
                              }
                              key
                            },
                            google_model = "embedding-001",
                            google_api_url_base = "https://generativelanguage.googleapis.com/v1beta/models/",
                            ...) {

  provider <- match.arg(provider)

  if (!is.character(texts) && !is.list(texts)) {
    stop("Input 'texts' must be a character vector or a list of strings.")
  }
  if (length(texts) == 0) {
    warning("Input 'texts' is empty. Returning NULL.")
    return(NULL)
  }
  if (is.list(texts)) {
    if (!all(vapply(texts, is.character, logical(1)))) {
      stop("If 'texts' is a list, all its elements must be character strings.")
    }
    if (!all(lengths(texts) == 1L)) {
      stop("If 'texts' is a list, each element must contain exactly one string.")
    }
    texts_list <- unname(texts)
    text_names <- vapply(texts, identity, character(1))
  } else {
    texts_list <- as.list(texts)
    text_names <- texts
  }

  build_openai_error <- function(error_body, status, sent_texts) {
    detailed_error_msg <- paste("OpenAI API Error (Status:", status, ")")
    problematic_input_info <- NULL

    if (!is.null(error_body$error)) {
      api_msg <- error_body$error$message
      api_type <- error_body$error$type
      api_param <- error_body$error$param
      api_code <- error_body$error$code

      msg_parts <- character()
      if (!is.null(api_type)) msg_parts <- c(msg_parts, paste("Type:", api_type))
      if (!is.null(api_code)) msg_parts <- c(msg_parts, paste("Code:", api_code))
      if (!is.null(api_param)) {
        msg_parts <- c(msg_parts, paste("Param:", api_param))
        if (grepl("input\\[([0-9]+)\\]", api_param)) {
          match_data <- regmatches(api_param, regexpr("input\\[([0-9]+)\\]", api_param))
          if (length(match_data) > 0) {
            index_str <- gsub("input\\[|\\]", "", match_data)
            err_index <- suppressWarnings(as.integer(index_str)) + 1L
            if (!is.na(err_index) && err_index > 0 && err_index <= length(sent_texts)) {
              problematic_input <- sent_texts[[err_index]]
              if (nchar(problematic_input) > 100) {
                problematic_input <- paste0(substr(problematic_input, 1, 100), "...")
              }
              problematic_input_info <- paste0(
                " Problematic Input (Index ", err_index, "): '", problematic_input, "'"
              )
            }
          }
        }
      }
      if (!is.null(api_msg)) msg_parts <- c(msg_parts, paste("Message:", api_msg))

      if (length(msg_parts) > 0) {
        detailed_error_msg <- paste(detailed_error_msg, "-", paste(msg_parts, collapse = ", "))
      }
    } else {
      detailed_error_msg <- paste(detailed_error_msg, "- No detailed error message provided by API.")
    }

    if (!is.null(problematic_input_info)) {
      detailed_error_msg <- paste0(detailed_error_msg, problematic_input_info)
    }

    detailed_error_msg
  }

  if (provider == "openai") {
    if (identical(api_key, "")) {
      stop("OpenAI API key not found. Set the OPENAI_API_KEY environment variable or pass via api_key argument.")
    }
    if (!is.character(model) || length(model) != 1) {
      stop("OpenAI 'model' must be a single string.")
    }
    if (!is.character(api_url) || length(api_url) != 1) {
      stop("OpenAI 'api_url' must be a single string.")
    }

    request_body <- list(input = texts_list, model = model)
    req <- httr2::request(api_url) |>
      httr2::req_method("POST") |>
      httr2::req_auth_bearer_token(token = api_key) |>
      httr2::req_headers("Content-Type" = "application/json") |>
      httr2::req_body_json(data = request_body) |>
      httr2::req_user_agent("wordfeatures R package (OpenAI embeddings)") |>
      httr2::req_retry(max_tries = 3, is_transient = ~ httr2::resp_status(.x) %in% c(429, 500, 502, 503, 504)) |>
      httr2::req_error(is_error = function(resp) FALSE)

    resp <- tryCatch(
      httr2::req_perform(req, ...),
      httr2_error = function(e) {
        rlang::abort(
          message = paste0("Embedding request failed: ", conditionMessage(e)),
          class = "wordfeatures_embedding_error",
          provider = provider,
          status = NA_integer_
        )
      }
    )

    api_status <- httr2::resp_status(resp)
    if (api_status >= 400) {
      error_body <- httr2::resp_body_json(resp, check_type = FALSE)
      rlang::abort(
        message = build_openai_error(error_body, api_status, texts_list),
        class = "wordfeatures_embedding_error",
        provider = provider,
        status = api_status
      )
    }

    response_body <- httr2::resp_body_json(resp)
    if (length(response_body$data) != length(texts_list)) {
      rlang::abort("Embedding count does not match input count.")
    }

    embeddings_list <- lapply(response_body$data, function(item) {
      if (is.null(item$embedding)) {
        rlang::abort("OpenAI response item is missing an embedding vector.")
      }
      as.numeric(item$embedding)
    })
  } else {
    if (identical(google_api_key, "")) {
      stop(
        "Google AI API key not found. Set the GOOGLE_API_KEY environment ",
        "variable or pass via google_api_key argument."
      )
    }
    if (!is.character(google_model) || length(google_model) != 1) {
      stop("Google 'google_model' must be a single string.")
    }
    if (!is.character(google_api_url_base) || length(google_api_url_base) != 1) {
      stop("Google 'google_api_url_base' must be a single string.")
    }

    google_model_api_name <- if (grepl("^models/", google_model)) {
      google_model
    } else {
      paste0("models/", google_model)
    }
    full_google_url <- paste0(
      google_api_url_base,
      google_model,
      ":batchEmbedContents?key=",
      google_api_key
    )

    google_requests <- lapply(texts_list, function(txt) {
      list(model = google_model_api_name, content = list(parts = list(list(text = txt))))
    })
    request_body <- list(requests = google_requests)

    req <- httr2::request(full_google_url) |>
      httr2::req_method("POST") |>
      httr2::req_headers("Content-Type" = "application/json") |>
      httr2::req_body_json(data = request_body) |>
      httr2::req_user_agent("wordfeatures R package (Google embeddings)") |>
      httr2::req_retry(max_tries = 3, is_transient = ~ httr2::resp_status(.x) %in% c(429, 500, 502, 503, 504)) |>
      httr2::req_error(is_error = function(resp) FALSE)

    resp <- tryCatch(
      httr2::req_perform(req, ...),
      httr2_error = function(e) {
        rlang::abort(
          message = paste0("Embedding request failed: ", conditionMessage(e)),
          class = "wordfeatures_embedding_error",
          provider = provider,
          status = NA_integer_
        )
      }
    )

    api_status <- httr2::resp_status(resp)
    if (api_status >= 400) {
      error_body <- httr2::resp_body_json(resp, check_type = FALSE)
      api_message <- if (!is.null(error_body$error) && !is.null(error_body$error$message)) {
        error_body$error$message
      } else {
        "Unknown Google AI API error."
      }
      rlang::abort(
        message = paste0("Embedding request failed: ", api_message),
        class = "wordfeatures_embedding_error",
        provider = provider,
        status = api_status
      )
    }

    response_body <- httr2::resp_body_json(resp)
    if (is.null(response_body$embeddings) || length(response_body$embeddings) != length(texts_list)) {
      rlang::abort("Embedding count does not match input count.")
    }

    embeddings_list <- lapply(response_body$embeddings, function(item) {
      if (is.null(item$values)) {
        rlang::abort("Google AI response item is missing an embedding vector.")
      }
      as.numeric(item$values)
    })
  }

  if (!all(vapply(embeddings_list, is.numeric, logical(1)))) {
    rlang::abort("All embeddings must be numeric vectors.")
  }
  dims <- vapply(embeddings_list, length, integer(1))
  if (length(unique(dims)) != 1L) {
    rlang::abort("Returned embeddings have inconsistent dimensions.")
  }

  names(embeddings_list) <- text_names
  embeddings_list
}
