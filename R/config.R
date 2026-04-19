# Defaults, limits, and shared UI constants (env-backed fields can be added later)
# Topic lists: see R/topics.R (`topic_choices`).

# `country_choices` is defined in R/countries.R (must be sourced before this file).

default_news_count <- function() 15L

news_count_limits <- function() {
  list(min = 5L, max = 50L)
}

#' Maximum allowed span of the news search period (calendar approximation).
#' @return Integer months (default 24).
max_period_months <- function() {
  v <- suppressWarnings(as.integer(Sys.getenv("NEWS_MAX_PERIOD_MONTHS", "24")))
  if (is.na(v) || v < 1L) 24L else v
}

#' Upper bound on period length in days derived from [max_period_months()] (31 days/month).
max_period_days <- function() {
  max_period_months() * 31L
}

#' TTL for request cache entries (seconds).
news_cache_ttl_sec <- function() {
  v <- suppressWarnings(as.numeric(Sys.getenv("NEWS_CACHE_TTL_SEC", "3600")))
  if (is.na(v) || v <= 0) 3600 else v
}

#' Directory for on-disk request cache (created on first write).
news_cache_dir <- function() {
  d <- Sys.getenv("NEWS_CACHE_DIR", "")
  if (nzchar(trimws(d))) {
    return(normalizePath(d, winslash = "/", mustWork = FALSE))
  }
  base <- tryCatch(
    tools::R_user_dir("news.collection", "cache"),
    error = function(e) file.path(tempdir(), "news.collection_cache")
  )
  normalizePath(base, winslash = "/", mustWork = FALSE)
}

#' Validate `news_count` against [news_count_limits()].
#'
#' @return `list(ok, n, errors)` where `n` is coerced integer when ok.
validate_news_count_input <- function(n) {
  lim <- news_count_limits()
  ni <- suppressWarnings(as.integer(n))
  if (length(ni) < 1L || is.na(ni[1L])) {
    return(list(ok = FALSE, n = NA_integer_, errors = "Invalid number of news items (N)."))
  }
  ni <- ni[1L]
  if (ni < lim$min || ni > lim$max) {
    return(list(
      ok = FALSE,
      n = ni,
      errors = sprintf("N must be between %d and %d.", lim$min, lim$max)
    ))
  }
  list(ok = TRUE, n = ni, errors = character())
}

#' Validate date range span against [max_period_days()].
#'
#' @return `list(ok, errors)`; `errors` empty if ok.
validate_period_span <- function(start, end) {
  if (is.null(start) || is.null(end)) {
    return(list(ok = FALSE, errors = "Period dates are missing."))
  }
  ds <- tryCatch(as.Date(start), error = function(e) NA)
  de <- tryCatch(as.Date(end), error = function(e) NA)
  if (any(is.na(c(ds, de)))) {
    return(list(ok = FALSE, errors = "Period dates are invalid."))
  }
  if (ds > de) {
    return(list(ok = FALSE, errors = "Start date must be on or before end date."))
  }
  span <- as.numeric(de - ds, units = "days")
  mx <- max_period_days()
  if (span > mx) {
    return(list(
      ok = FALSE,
      errors = sprintf(
        "Period is too long (%.0f days). Maximum is about %d months (%d days). Narrow the date range.",
        span,
        max_period_months(),
        mx
      )
    ))
  }
  list(ok = TRUE, errors = character())
}

default_date_range <- function() {
  end <- Sys.Date()
  start <- seq(from = end, length.out = 2, by = "-6 months")[2]
  c(start = start, end = end)
}

max_followup_lines <- 10L
max_followup_chars <- 200L

# OpenAI (Responses API client: see R/openai_client.R)

openai_api_key <- function() {
  k <- Sys.getenv("OPENAI_API_KEY", "")
  if (!nzchar(k)) NA_character_ else k
}

openai_default_model <- function() {
  m <- Sys.getenv("OPENAI_MODEL", "gpt-5.4")
  if (!nzchar(m)) "gpt-5.4" else m
}

openai_http_timeout_sec <- function() {
  v <- suppressWarnings(as.numeric(Sys.getenv("OPENAI_TIMEOUT_SEC", "120")))
  if (is.na(v) || v <= 0) 120 else v
}

openai_max_retries <- function() {
  v <- suppressWarnings(as.integer(Sys.getenv("OPENAI_MAX_RETRIES", "4")))
  if (is.na(v) || v < 1L) 4L else v
}

#' @return `TRUE` if `OPENAI_API_KEY` is non-empty (safe to call the API).
openai_has_api_key <- function() {
  k <- openai_api_key()
  !is.na(k) && nzchar(k)
}

#' Cap for Responses API output size (large N × long articles need headroom).
openai_max_output_tokens <- function() {
  v <- suppressWarnings(as.integer(Sys.getenv("OPENAI_MAX_OUTPUT_TOKENS", "32768")))
  if (is.na(v) || v < 2048L) 32768L else v
}

#' Attach `web_search_preview` tool; set `OPENAI_WEB_SEARCH=false` if the API returns 403 for tools.
openai_web_search_enabled <- function() {
  v <- tolower(trimws(Sys.getenv("OPENAI_WEB_SEARCH", "true")))
  !v %in% c("0", "false", "no", "off")
}
