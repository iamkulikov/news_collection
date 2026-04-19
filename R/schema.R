# Validation for sovereign-news JSON (post-parse), aligned with `news_response_json_schema()` in R/prompt.R.
# Keep `VALID_*` in sync with `topic_category_values` in R/prompt.R.

VALID_TOPIC_CATEGORIES <- c(
  "macro", "budget", "public_sector", "external", "institutions",
  "research_ifis", "follow_up_custom"
)

VALID_TOPICS_FIELD_VALUES <- setdiff(VALID_TOPIC_CATEGORIES, "follow_up_custom")

.is_nonempty_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

.is_url_like <- function(x) {
  if (!.is_nonempty_string(x)) return(FALSE)
  grepl("^https?://[^[:space:]]+", x, ignore.case = TRUE)
}

#' Validate parsed sovereign-news JSON (from `jsonlite::fromJSON`, `simplifyVector = FALSE`).
#'
#' @param parsed R object (typically a `list`).
#' @param n_expected Optional integer: required length of `items` (same as request `N`).
#' @return Named list: `ok` (logical), `errors` (character vector, empty if ok).
validate_news_response <- function(parsed, n_expected = NULL) {
  errs <- character()

  push <- function(...) {
    errs <<- c(errs, sprintf(...))
  }

  if (!is.list(parsed)) {
    return(list(ok = FALSE, errors = "Root JSON value must be an object (list)."))
  }

  req_top <- c("country", "time_window", "topics", "follow_up_queries", "items")
  miss <- setdiff(req_top, names(parsed))
  if (length(miss)) {
    push("Missing top-level keys: %s", paste(miss, collapse = ", "))
    return(list(ok = FALSE, errors = errs))
  }

  co <- parsed$country
  if (!is.list(co)) {
    push("`country` must be an object.")
  } else {
    if (!.is_nonempty_string(co$iso3) || nchar(co$iso3) != 3L) {
      push("`country.iso3` must be a 3-letter ISO3 string.")
    }
    if (!.is_nonempty_string(co$display_name)) {
      push("`country.display_name` must be a non-empty string.")
    }
  }

  tw <- parsed$time_window
  if (!is.list(tw)) {
    push("`time_window` must be an object.")
  } else {
    if (!.is_nonempty_string(tw$start)) push("`time_window.start` must be a non-empty string (ISO date).")
    if (!.is_nonempty_string(tw$end)) push("`time_window.end` must be a non-empty string (ISO date).")
  }

  tp <- parsed$topics
  if (!is.list(tp) && !is.character(tp)) {
    push("`topics` must be an array of strings.")
  } else {
    tvec <- if (is.character(tp)) as.list(tp) else tp
    for (i in seq_along(tvec)) {
      ti <- tvec[[i]]
      if (!.is_nonempty_string(ti) || !ti %in% VALID_TOPICS_FIELD_VALUES) {
        push("`topics[%d]` must be one of the allowed topic ids.", i)
      }
    }
  }

  fu <- parsed$follow_up_queries
  if (!is.list(fu) && !is.character(fu)) {
    push("`follow_up_queries` must be an array of strings.")
  } else {
    fvec <- if (is.character(fu)) as.list(fu) else fu
    for (i in seq_along(fvec)) {
      if (!is.character(fvec[[i]]) || length(fvec[[i]]) != 1L) {
        push("`follow_up_queries[%d]` must be a string.", i)
      }
    }
  }

  items <- parsed$items
  if (!is.list(items)) {
    push("`items` must be an array.")
    return(list(ok = FALSE, errors = errs))
  }

  n_items <- length(items)
  if (!is.null(n_expected)) {
    n_expected <- as.integer(n_expected)[1L]
    if (!is.na(n_expected) && n_items != n_expected) {
      push("`items` must have length %d (got %d).", n_expected, n_items)
    }
  }

  topic_cat_allowed <- VALID_TOPIC_CATEGORIES
  score_names <- c("credit_importance", "visibility", "freshness")

  for (j in seq_len(n_items)) {
    it <- items[[j]]
    pref <- sprintf("items[%d]", j)
    if (!is.list(it)) {
      push("%s must be an object.", pref)
      next
    }
    req_i <- c(
      "title", "date", "what_happened", "why_it_matters_for_sovereign_risk",
      "topic_category", "scores", "composite_rank", "sources", "origin", "tags",
      "notes_on_evidence_quality"
    )
    miss_i <- setdiff(req_i, names(it))
    if (length(miss_i)) {
      push("%s: missing keys: %s", pref, paste(miss_i, collapse = ", "))
    }
    if (!.is_nonempty_string(it$title)) push("%s.title must be a non-empty string.", pref)
    if (!is.character(it$date) || length(it$date) != 1L || is.na(it$date)) {
      push("%s.date must be a string.", pref)
    }
    if (!.is_nonempty_string(it$what_happened)) push("%s.what_happened must be a non-empty string.", pref)
    if (!.is_nonempty_string(it$why_it_matters_for_sovereign_risk)) {
      push("%s.why_it_matters_for_sovereign_risk must be a non-empty string.", pref)
    }
    if (!.is_nonempty_string(it$topic_category) || !it$topic_category %in% topic_cat_allowed) {
      push("%s.topic_category must be a valid category.", pref)
    }

    sc <- it$scores
    if (!is.list(sc)) {
      push("%s.scores must be an object.", pref)
    } else {
      for (sn in score_names) {
        v <- sc[[sn]]
        ok_num <- (is.numeric(v) || is.integer(v)) && length(v) == 1L && !is.na(v)
        if (!ok_num || v < 1 || v > 5 || (abs(v - round(v)) > 1e-9)) {
          push("%s.scores.%s must be an integer 1–5.", pref, sn)
        }
      }
    }

    cr <- it$composite_rank
    ok_cr <- (is.numeric(cr) || is.integer(cr)) && length(cr) == 1L && !is.na(cr)
    if (!ok_cr || cr < 1 || cr > n_items || (abs(cr - round(cr)) > 1e-9)) {
      push("%s.composite_rank must be an integer from 1 to %d.", pref, max(1L, n_items))
    }

    src <- it$sources
    if (!is.list(src)) {
      push("%s.sources must be an array.", pref)
    } else {
      ns <- length(src)
      if (ns < 2L || ns > 6L) {
        push("%s.sources must have 2–6 entries (got %d).", pref, ns)
      }
      for (k in seq_len(ns)) {
        s <- src[[k]]
        if (!is.list(s)) {
          push("%s.sources[%d] must be an object.", pref, k)
          next
        }
        if (!.is_url_like(s$url)) push("%s.sources[%d].url must be an http(s) URL.", pref, k)
        if (!.is_nonempty_string(s$source_name)) push("%s.sources[%d].source_name must be a non-empty string.", pref, k)
        if ("published_date" %in% names(s)) {
          pd <- s$published_date
          if (!(is.character(pd) && length(pd) == 1L && !is.na(pd))) {
            push("%s.sources[%d].published_date must be a string if present.", pref, k)
          }
        }
      }
    }

    if (!.is_nonempty_string(it$origin) || !it$origin %in% c("base_search", "follow_up")) {
      push("%s.origin must be base_search or follow_up.", pref)
    }

    tg <- it$tags
    if (!is.list(tg) && !is.character(tg)) {
      push("%s.tags must be an array of strings.", pref)
    } else {
      tgv <- if (is.character(tg)) as.list(tg) else tg
      for (ti in seq_along(tgv)) {
        if (!is.character(tgv[[ti]]) || length(tgv[[ti]]) != 1L) {
          push("%s.tags[%d] must be a string.", pref, ti)
        }
      }
    }

    if (!is.character(it$notes_on_evidence_quality) || length(it$notes_on_evidence_quality) != 1L ||
        is.na(it$notes_on_evidence_quality)) {
      push("%s.notes_on_evidence_quality must be a string (may be empty).", pref)
    }
  }

  if (n_items >= 1L) {
    ranks <- vapply(items, function(it) {
      cr <- it$composite_rank
      if ((is.numeric(cr) || is.integer(cr)) && length(cr) == 1L && !is.na(cr)) as.numeric(cr) else NA_real_
    }, numeric(1))
    if (anyNA(ranks)) {
      # already reported
    } else {
      rint <- as.integer(round(ranks))
      exp <- sort(unique(rint))
      if (length(exp) != n_items || !identical(sort(exp), seq_len(n_items))) {
        push("`items[*].composite_rank` must be a permutation of 1..%d (unique).", n_items)
      }
    }
  }

  list(ok = length(errs) == 0L, errors = errs)
}
