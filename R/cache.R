# Disk-backed request cache: key = SHA-256 of canonical request parameters + model.

#' Build stable cache key (hex digest) for a sovereign-news request.
#'
#' Key inputs match the plan: country ISO3, period, topics, follow-up queries, N, model.
#'
#' @param iso3 ISO3 country code.
#' @param start,end Period bounds (Date-coercible).
#' @param topics Character vector of topic ids (order ignored).
#' @param follow_up_queries Character vector (order ignored).
#' @param n Requested number of news items.
#' @param model OpenAI model id.
#' @param report_language Report language (`RUS`/`ENG`), included in the key.
#' @return Character scalar: 64-char hex SHA-256.
news_request_cache_key <- function(iso3, start, end, topics, follow_up_queries, n, model, report_language = "RUS") {
  iso3 <- toupper(trimws(as.character(iso3)))
  start <- as.character(as.Date(start))
  end <- as.character(as.Date(end))
  topics <- sort(unique(as.character(topics)))
  topics <- topics[!is.na(topics) & nzchar(topics)]
  fu <- sort(unique(as.character(follow_up_queries)))
  fu <- fu[!is.na(fu) & nzchar(fu)]
  n <- as.integer(n)[1L]
  model <- as.character(model)[1L]
  report_language <- toupper(trimws(as.character(report_language)[1L]))
  if (!report_language %in% c("RUS", "ENG")) {
    report_language <- "RUS"
  }
  payload <- list(
    v = 3L,
    iso3 = iso3,
    start = start,
    end = end,
    topics = topics,
    follow_up = fu,
    n = n,
    model = model,
    report_language = report_language
  )
  digest::digest(payload, algo = "sha256")
}

.news_cache_path <- function(key) {
  d <- news_cache_dir()
  file.path(d, paste0(key, ".rds"))
}

#' Read cached payload if present and not expired.
#'
#' @param key From [news_request_cache_key()].
#' @return `NULL` on miss; otherwise `list(parsed = ..., usage = ..., saved_at = POSIXct)`.
news_cache_get <- function(key) {
  if (!is.character(key) || !nzchar(key)) {
    return(NULL)
  }
  path <- .news_cache_path(key)
  if (!file.exists(path)) {
    return(NULL)
  }
  ent <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(ent) || is.null(ent$saved_at)) {
    return(NULL)
  }
  ttl <- if (!is.null(ent$ttl_sec) && is.numeric(ent$ttl_sec) && ent$ttl_sec[1L] > 0) {
    ent$ttl_sec[1L]
  } else {
    news_cache_ttl_sec()
  }
  age <- as.numeric(difftime(Sys.time(), ent$saved_at, units = "secs"))
  if (!is.finite(age) || age > ttl) {
    unlink(path, force = TRUE)
    return(NULL)
  }
  if (is.null(ent$parsed)) {
    return(NULL)
  }
  list(
    parsed = ent$parsed,
    usage = ent$usage,
    saved_at = ent$saved_at
  )
}

#' Store validated response (and optional API usage) under cache key.
#'
#' @param key Cache key.
#' @param parsed Object validated by [validate_news_response()].
#' @param usage Optional list from [openai_responses_extract_usage()] or `NULL`.
news_cache_set <- function(key, parsed, usage = NULL) {
  if (!is.character(key) || !nzchar(key)) {
    return(invisible(FALSE))
  }
  d <- news_cache_dir()
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  path <- .news_cache_path(key)
  ent <- list(
    saved_at = Sys.time(),
    ttl_sec = news_cache_ttl_sec(),
    parsed = parsed,
    usage = usage
  )
  saveRDS(ent, path)
  invisible(TRUE)
}
