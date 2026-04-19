# Render validated sovereign-news JSON to Shiny UI + CSV flattening for export.
# Expects structure validated by `validate_news_response()` in R/schema.R.

.topic_category_display_name <- function(cat) {
  cat <- as.character(cat)[1L]
  if (identical(cat, "follow_up_custom")) {
    return("Follow-up (custom)")
  }
  w <- which(topic_choices == cat)
  if (length(w) == 1L) {
    return(names(topic_choices)[w])
  }
  cat
}


.origin_display_name <- function(origin) {
  if (identical(origin, "base_search")) {
    return("Base search")
  }
  if (identical(origin, "follow_up")) {
    return("Follow-up")
  }
  as.character(origin)[1L]
}


.coerce_string_list <- function(x) {
  if (is.null(x)) {
    return(character())
  }
  if (is.character(x)) {
    return(x[!is.na(x)])
  }
  if (!is.list(x)) {
    return(character())
  }
  vapply(x, function(z) {
    if (is.character(z) && length(z) == 1L) z else as.character(z)[1L]
  }, character(1))
}


#' Sort `items` by `composite_rank` ascending (1 = top).
#'
#' @param items List of item objects.
#' @return Reordered list.
sort_items_by_composite_rank <- function(items) {
  if (!length(items)) {
    return(items)
  }
  ranks <- vapply(items, function(it) {
    cr <- it$composite_rank
    if ((is.numeric(cr) || is.integer(cr)) && length(cr) == 1L && !is.na(cr)) {
      return(as.numeric(cr))
    }
    NA_real_
  }, numeric(1))
  ord <- order(ranks, na.last = TRUE, method = "auto")
  items[ord]
}


.score_int <- function(scores, field) {
  if (!is.list(scores)) {
    return(NA_integer_)
  }
  v <- scores[[field]]
  if (is.null(v) || length(v) != 1L) {
    return(NA_integer_)
  }
  suppressWarnings(as.integer(round(as.numeric(v))))
}


.format_multiline_text <- function(text) {
  text <- as.character(text)[1L]
  if (is.na(text) || !nzchar(text)) {
    return(tags$p(class = "text-muted mb-0", "—"))
  }
  parts <- strsplit(text, "\n", fixed = TRUE)[[1]]
  parts <- parts[nzchar(trimws(parts))]
  if (length(parts) <= 1L) {
    return(tags$p(class = "mb-0", text))
  }
  tags$ul(class = "mb-0 ps-3", lapply(parts, function(line) tags$li(line)))
}


#' Build Shiny UI for one news item card (bslib).
#'
#' @param it Item list.
#' @return Shiny tag list.
news_item_card_ui <- function(it) {
  sc <- it$scores
  c_imp <- .score_int(sc, "credit_importance")
  vis <- .score_int(sc, "visibility")
  fr <- .score_int(sc, "freshness")
  cr <- it$composite_rank
  cr_lab <- if ((is.numeric(cr) || is.integer(cr)) && length(cr) == 1L && !is.na(cr)) {
    as.integer(round(as.numeric(cr)))
  } else {
    NA_integer_
  }

  topic_lab <- .topic_category_display_name(it$topic_category)
  origin_lab <- .origin_display_name(it$origin)
  date_chr <- as.character(it$date)[1L]
  if (is.na(date_chr)) {
    date_chr <- ""
  }

  src <- it$sources
  src_list <- if (is.list(src)) src else list()

  link_items <- lapply(seq_along(src_list), function(k) {
    s <- src_list[[k]]
    if (!is.list(s)) {
      return(NULL)
    }
    url <- as.character(s$url)[1L]
    nm <- as.character(s$source_name)[1L]
    pd <- s$published_date
    pd_chr <- if (!is.null(pd)) as.character(pd)[1L] else ""
    if (is.na(url) || !nzchar(url)) {
      return(NULL)
    }
    line <- tags$li(
      tags$a(href = url, target = "_blank", rel = "noopener noreferrer", nm),
      if (is.character(pd_chr) && nzchar(pd_chr) && !is.na(pd_chr)) {
        tags$span(class = "text-muted", " — ", pd_chr)
      }
    )
    line
  })
  link_items <- link_items[!vapply(link_items, is.null, logical(1))]

  item_tag_list <- .coerce_string_list(it$tags)
  notes <- as.character(it$notes_on_evidence_quality)[1L]
  if (is.na(notes)) {
    notes <- ""
  }

  score_line <- sprintf(
    "Credit importance: %s · Visibility: %s · Freshness: %s",
    c_imp, vis, fr
  )

  card(
    class = "mb-3 border shadow-sm",
    card_header(
      class = "py-2",
      tags$div(
        class = "d-flex flex-wrap justify-content-between align-items-start gap-2",
        tags$div(
          tags$h5(class = "mb-1", as.character(it$title)[1L]),
          if (nzchar(date_chr)) {
            tags$small(class = "text-muted d-block", date_chr)
          }
        ),
        tags$div(
          class = "text-end",
          if (!is.na(cr_lab)) {
            tags$span(class = "badge bg-primary me-1", paste0("#", cr_lab))
          },
          tags$span(class = "badge bg-secondary me-1", topic_lab),
          tags$span(class = "badge bg-info text-dark", origin_lab)
        )
      )
    ),
    card_body(
      tags$p(class = "small text-muted mb-2", score_line),
      tags$h6(class = "text-uppercase small text-muted", "What happened"),
      .format_multiline_text(it$what_happened),
      tags$h6(class = "text-uppercase small text-muted mt-3", "Why it matters for sovereign risk"),
      .format_multiline_text(it$why_it_matters_for_sovereign_risk),
      tags$h6(class = "text-uppercase small text-muted mt-3", "Sources"),
      if (length(link_items)) {
        tags$ul(class = "mb-0", link_items)
      } else {
        tags$p(class = "text-muted mb-0", "No links.")
      },
      if (length(item_tag_list)) {
        tags$div(
          class = "mt-2",
          tags$span(class = "small text-muted", "Tags: "),
          tags$span(class = "small", paste(item_tag_list, collapse = ", "))
        )
      },
      if (nzchar(notes)) {
        tags$div(
          class = "mt-2 small text-muted border-top pt-2",
          tags$strong("Evidence quality: "),
          notes
        )
      }
    )
  )
}


#' Full results panel: metadata + cards for each item.
#'
#' @param parsed Validated root object (`country`, `time_window`, `items`, ...).
#' @return `shiny.tag` tree.
render_news_response_ui <- function(parsed) {
  if (is.null(parsed)) {
    return(tags$p(class = "text-muted", "No run yet."))
  }

  co <- parsed$country
  iso3 <- if (is.list(co)) as.character(co$iso3)[1L] else ""
  dname <- if (is.list(co)) as.character(co$display_name)[1L] else ""
  tw <- parsed$time_window
  t0 <- if (is.list(tw)) as.character(tw$start)[1L] else ""
  t1 <- if (is.list(tw)) as.character(tw$end)[1L] else ""

  items <- parsed$items
  if (!is.list(items)) {
    items <- list()
  }
  items <- sort_items_by_composite_rank(items)

  meta <- tags$div(
    class = "mb-3 p-2 bg-light rounded small",
    tags$strong(dname, " (", iso3, ")"),
    " · ",
    t0, " → ", t1
  )

  if (!length(items)) {
    return(tags$div(meta, tags$div(class = "alert alert-info mb-0", "No news items in this response.")))
  }

  cards <- lapply(items, news_item_card_ui)
  tags$div(class = "news-results", meta, cards)
}


#' Flatten validated response to a single data.frame (one row per news item).
#'
#' @param parsed Validated root list.
#' @return A `data.frame`.
news_response_to_csv_df <- function(parsed) {
  if (is.null(parsed) || !is.list(parsed)) {
    return(data.frame())
  }

  co <- parsed$country
  iso3 <- if (is.list(co)) as.character(co$iso3)[1L] else NA_character_
  dname <- if (is.list(co)) as.character(co$display_name)[1L] else NA_character_

  tw <- parsed$time_window
  t0 <- if (is.list(tw)) as.character(tw$start)[1L] else NA_character_
  t1 <- if (is.list(tw)) as.character(tw$end)[1L] else NA_character_

  tp <- parsed$topics
  topic_vec <- if (is.character(tp)) {
    tp
  } else {
    .coerce_string_list(tp)
  }
  topics_joined <- paste(topic_vec, collapse = ";")

  fu <- parsed$follow_up_queries
  fu_vec <- .coerce_string_list(fu)
  fu_joined <- paste(fu_vec, collapse = ";")

  items <- parsed$items
  if (!is.list(items) || !length(items)) {
    return(data.frame(
      country_iso3 = character(),
      country_display_name = character(),
      period_start = character(),
      period_end = character(),
      topics_requested = character(),
      follow_up_queries = character(),
      composite_rank = integer(),
      title = character(),
      date = character(),
      topic_category = character(),
      origin = character(),
      score_credit_importance = integer(),
      score_visibility = integer(),
      score_freshness = integer(),
      what_happened = character(),
      why_it_matters_for_sovereign_risk = character(),
      sources_urls = character(),
      sources_names = character(),
      tags = character(),
      notes_on_evidence_quality = character(),
      stringsAsFactors = FALSE
    ))
  }

  n <- length(items)
  out <- vector("list", n)

  for (j in seq_len(n)) {
    it <- items[[j]]
    sc <- if (is.list(it)) it$scores else list()
    src <- if (is.list(it) && is.list(it$sources)) it$sources else list()
    urls <- vapply(src, function(s) {
      if (!is.list(s)) {
        return("")
      }
      as.character(s$url)[1L]
    }, character(1))
    snames <- vapply(src, function(s) {
      if (!is.list(s)) {
        return("")
      }
      as.character(s$source_name)[1L]
    }, character(1))

    tg <- .coerce_string_list(if (is.list(it)) it$tags else NULL)

    out[[j]] <- data.frame(
      country_iso3 = iso3,
      country_display_name = dname,
      period_start = t0,
      period_end = t1,
      topics_requested = topics_joined,
      follow_up_queries = fu_joined,
      composite_rank = {
        cr <- it$composite_rank
        if ((is.numeric(cr) || is.integer(cr)) && length(cr) == 1L && !is.na(cr)) {
          as.integer(round(as.numeric(cr)))
        } else {
          NA_integer_
        }
      },
      title = as.character(it$title)[1L],
      date = as.character(it$date)[1L],
      topic_category = as.character(it$topic_category)[1L],
      origin = as.character(it$origin)[1L],
      score_credit_importance = .score_int(sc, "credit_importance"),
      score_visibility = .score_int(sc, "visibility"),
      score_freshness = .score_int(sc, "freshness"),
      what_happened = as.character(it$what_happened)[1L],
      why_it_matters_for_sovereign_risk = as.character(it$why_it_matters_for_sovereign_risk)[1L],
      sources_urls = paste(urls, collapse = " | "),
      sources_names = paste(snames, collapse = " | "),
      tags = paste(tg, collapse = ";"),
      notes_on_evidence_quality = as.character(it$notes_on_evidence_quality)[1L],
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, out)
}


#' Build a minimal valid response with no items (for UI wiring before API data exists).
#'
#' @param iso3 ISO3 country code.
#' @param date_start,date_end Date or character coercible to Date.
#' @param topic_ids Character vector of topic ids.
#' @param follow_up_queries Character vector.
#' @return Named list matching the sovereign-news JSON contract.
build_empty_validated_news_response <- function(iso3, date_start, date_end, topic_ids, follow_up_queries) {
  iso3 <- toupper(trimws(as.character(iso3)))
  dn <- countrycode::countrycode(iso3, "iso3c", "country.name.en", warn = FALSE)
  if (length(dn) != 1L || is.na(dn)) {
    dn <- iso3
  }
  topic_ids <- unique(as.character(topic_ids))
  topic_ids <- topic_ids[!is.na(topic_ids) & nzchar(topic_ids)]
  follow_up_queries <- unique(as.character(follow_up_queries))
  follow_up_queries <- follow_up_queries[!is.na(follow_up_queries) & nzchar(follow_up_queries)]

  list(
    country = list(iso3 = iso3, display_name = as.character(dn)),
    time_window = list(
      start = as.character(as.Date(date_start)),
      end = as.character(as.Date(date_end))
    ),
    topics = as.list(topic_ids),
    follow_up_queries = as.list(follow_up_queries),
    items = list()
  )
}


.docx_add_text_block <- function(doc, label, body_text) {
  doc <- officer::body_add_par(doc, label, style = "heading 3")
  txt <- as.character(body_text)[1L]
  if (is.na(txt) || !nzchar(txt)) {
    return(officer::body_add_par(doc, "—", style = "Normal"))
  }
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) {
    return(officer::body_add_par(doc, "—", style = "Normal"))
  }
  for (ln in lines) {
    doc <- officer::body_add_par(doc, ln, style = "Normal")
  }
  doc
}


#' Write validated news response to a Word (.docx) file (same content as cards/CSV).
#'
#' @param parsed Validated root list.
#' @param path Destination `.docx` path.
#' @return Invisibly `path`.
write_news_response_docx <- function(parsed, path) {
  if (is.null(parsed) || !is.list(parsed)) {
    stop("`parsed` must be a non-empty list.", call. = FALSE)
  }

  co <- parsed$country
  iso3 <- if (is.list(co)) as.character(co$iso3)[1L] else ""
  dname <- if (is.list(co)) as.character(co$display_name)[1L] else ""
  tw <- parsed$time_window
  t0 <- if (is.list(tw)) as.character(tw$start)[1L] else ""
  t1 <- if (is.list(tw)) as.character(tw$end)[1L] else ""

  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "Sovereign risk — economic news", style = "heading 1")
  doc <- officer::body_add_par(
    doc,
    paste0(dname, " (", iso3, ") — period: ", t0, " → ", t1),
    style = "Normal"
  )
  doc <- officer::body_add_par(doc, "", style = "Normal")

  items <- parsed$items
  if (!is.list(items)) {
    items <- list()
  }
  items <- sort_items_by_composite_rank(items)

  if (!length(items)) {
    doc <- officer::body_add_par(doc, "No news items in this response.", style = "Normal")
    print(doc, target = path)
    return(invisible(path))
  }

  for (it in items) {
    title <- as.character(it$title)[1L]
    doc <- officer::body_add_par(doc, title, style = "heading 2")

    date_chr <- as.character(it$date)[1L]
    if (!is.na(date_chr) && nzchar(date_chr)) {
      doc <- officer::body_add_par(doc, paste("Date:", date_chr), style = "Normal")
    }

    topic_lab <- .topic_category_display_name(it$topic_category)
    origin_lab <- .origin_display_name(it$origin)
    cr <- it$composite_rank
    cr_txt <- if ((is.numeric(cr) || is.integer(cr)) && length(cr) == 1L && !is.na(cr)) {
      paste0("Rank #", as.integer(round(as.numeric(cr))))
    } else {
      ""
    }
    meta_bits <- c(
      paste("Topic:", topic_lab),
      paste("Origin:", origin_lab),
      if (nzchar(cr_txt)) cr_txt
    )
    doc <- officer::body_add_par(doc, paste(meta_bits, collapse = " · "), style = "Normal")

    sc <- if (is.list(it)) it$scores else list()
    doc <- officer::body_add_par(
      doc,
      sprintf(
        "Scores — credit importance: %s, visibility: %s, freshness: %s",
        .score_int(sc, "credit_importance"),
        .score_int(sc, "visibility"),
        .score_int(sc, "freshness")
      ),
      style = "Normal"
    )

    doc <- .docx_add_text_block(doc, "What happened", it$what_happened)
    doc <- .docx_add_text_block(doc, "Why it matters for sovereign risk", it$why_it_matters_for_sovereign_risk)

    doc <- officer::body_add_par(doc, "Sources", style = "heading 3")
    src <- if (is.list(it)) it$sources else list()
    if (!is.list(src) || !length(src)) {
      doc <- officer::body_add_par(doc, "No links.", style = "Normal")
    } else {
      for (s in src) {
        if (!is.list(s)) {
          next
        }
        url <- as.character(s$url)[1L]
        nm <- as.character(s$source_name)[1L]
        pd <- s$published_date
        pd_chr <- if (!is.null(pd)) as.character(pd)[1L] else ""
        line <- paste0(nm, " — ", url)
        if (is.character(pd_chr) && nzchar(pd_chr) && !is.na(pd_chr)) {
          line <- paste0(line, " (", pd_chr, ")")
        }
        doc <- officer::body_add_par(doc, line, style = "Normal")
      }
    }

    item_tag_list <- .coerce_string_list(if (is.list(it)) it$tags else NULL)
    if (length(item_tag_list)) {
      doc <- officer::body_add_par(
        doc,
        paste("Tags:", paste(item_tag_list, collapse = ", ")),
        style = "Normal"
      )
    }

    notes <- as.character(it$notes_on_evidence_quality)[1L]
    if (!is.na(notes) && nzchar(notes)) {
      doc <- officer::body_add_par(doc, paste("Evidence quality:", notes), style = "Normal")
    }

    doc <- officer::body_add_par(doc, "", style = "Normal")
  }

  print(doc, target = path)
  invisible(path)
}
