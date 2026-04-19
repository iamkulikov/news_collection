# Render validated sovereign-news JSON to Shiny UI + CSV flattening for export.
# Expects structure validated by `validate_news_response()` in R/schema.R.

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
news_item_card_ui <- function(it, report_lang = "RUS") {
  report_lang <- normalize_report_language(report_lang)
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

  topic_lab <- topic_display_name(it$topic_category, report_lang = report_lang)
  origin_lab <- origin_display_name(it$origin, report_lang = report_lang)
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
    tr(report_lang, "score_line"),
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
      tags$h6(class = "text-uppercase small text-muted", tr(report_lang, "what_happened")),
      .format_multiline_text(it$what_happened),
      tags$h6(class = "text-uppercase small text-muted mt-3", tr(report_lang, "why_matters")),
      .format_multiline_text(it$why_it_matters_for_sovereign_risk),
      tags$h6(class = "text-uppercase small text-muted mt-3", tr(report_lang, "sources")),
      if (length(link_items)) {
        tags$ul(class = "mb-0", link_items)
      } else {
        tags$p(class = "text-muted mb-0", tr(report_lang, "no_links"))
      },
      if (length(item_tag_list)) {
        tags$div(
          class = "mt-2",
          tags$span(class = "small text-muted", tr(report_lang, "tags")),
          tags$span(class = "small", paste(item_tag_list, collapse = ", "))
        )
      },
      if (nzchar(notes)) {
        tags$div(
          class = "mt-2 small text-muted border-top pt-2",
          tags$strong(tr(report_lang, "evidence_quality")),
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
render_news_response_ui <- function(parsed, report_lang = "RUS") {
  report_lang <- normalize_report_language(report_lang)
  if (is.null(parsed)) {
    return(tags$p(class = "text-muted", tr(report_lang, "no_run")))
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
    return(tags$div(meta, tags$div(class = "alert alert-info mb-0", tr(report_lang, "no_items"))))
  }

  cards <- lapply(items, function(it) news_item_card_ui(it, report_lang = report_lang))
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


.docx_font_family <- function() {
  # Calibri is the primary requirement; Word will pick a local fallback (typically Arial) when unavailable.
  "Calibri"
}


.docx_text_prop <- function(size = 11, bold = FALSE, italic = FALSE, underline = FALSE, color = "#000000") {
  officer::fp_text(
    font.size = size,
    font.family = .docx_font_family(),
    bold = bold,
    italic = italic,
    underlined = underline,
    color = color
  )
}


.docx_add_line <- function(doc, text, size = 11, bold = FALSE) {
  p <- officer::fpar(
    officer::ftext(as.character(text)[1L], prop = .docx_text_prop(size = size, bold = bold))
  )
  officer::body_add_fpar(doc, p, style = "Normal")
}


.docx_format_date <- function(x) {
  x_chr <- as.character(x)[1L]
  if (is.na(x_chr) || !nzchar(x_chr)) {
    return("")
  }
  x_date <- suppressWarnings(as.Date(x_chr))
  if (is.na(x_date)) {
    return(x_chr)
  }
  format(x_date, "%d.%m.%Y")
}


.docx_add_text_block <- function(doc, label, body_text) {
  doc <- .docx_add_line(doc, label, size = 12, bold = TRUE)
  txt <- as.character(body_text)[1L]
  if (is.na(txt) || !nzchar(txt)) {
    return(.docx_add_line(doc, "—"))
  }
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) {
    return(.docx_add_line(doc, "—"))
  }
  for (ln in lines) {
    doc <- .docx_add_line(doc, ln)
  }
  doc
}


#' Write validated news response to a Word (.docx) file (same content as cards/CSV).
#'
#' @param parsed Validated root list.
#' @param path Destination `.docx` path.
#' @return Invisibly `path`.
write_news_response_docx <- function(parsed, path, report_lang = "RUS") {
  report_lang <- normalize_report_language(report_lang)
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
  country_lab <- trimws(as.character(dname)[1L])
  if (is.na(country_lab) || !nzchar(country_lab)) {
    country_lab <- trimws(as.character(iso3)[1L])
  }
  period_start <- .docx_format_date(t0)
  period_end <- .docx_format_date(t1)
  header_line <- sprintf(
    "%s Economic News - %s to %s",
    country_lab,
    period_start,
    period_end
  )
  doc <- .docx_add_line(doc, header_line, size = 16, bold = TRUE)
  doc <- .docx_add_line(doc, "")

  items <- parsed$items
  if (!is.list(items)) {
    items <- list()
  }
  items <- sort_items_by_composite_rank(items)

  if (!length(items)) {
    doc <- .docx_add_line(doc, tr(report_lang, "no_items"))
    print(doc, target = path)
    return(invisible(path))
  }

  for (idx in seq_along(items)) {
    it <- items[[idx]]
    title <- as.character(it$title)[1L]
    title_line <- sprintf("%d. %s", idx, title)
    doc <- .docx_add_line(doc, title_line, size = 14, bold = TRUE)

    date_chr <- as.character(it$date)[1L]
    if (!is.na(date_chr) && nzchar(date_chr)) {
      doc <- .docx_add_line(doc, paste(tr(report_lang, "date"), .docx_format_date(date_chr)))
    }

    topic_lab <- topic_display_name(it$topic_category, report_lang = report_lang)
    origin_lab <- origin_display_name(it$origin, report_lang = report_lang)
    cr <- it$composite_rank
    cr_txt <- if ((is.numeric(cr) || is.integer(cr)) && length(cr) == 1L && !is.na(cr)) {
      sprintf(tr(report_lang, "rank"), as.integer(round(as.numeric(cr))))
    } else {
      ""
    }
    meta_bits <- c(
      paste(tr(report_lang, "topic"), topic_lab),
      paste(tr(report_lang, "origin"), origin_lab),
      if (nzchar(cr_txt)) cr_txt
    )
    doc <- .docx_add_line(doc, paste(meta_bits, collapse = " · "))

    sc <- if (is.list(it)) it$scores else list()
    doc <- .docx_add_line(
      doc,
      sprintf(
        tr(report_lang, "scores"),
        .score_int(sc, "credit_importance"),
        .score_int(sc, "visibility"),
        .score_int(sc, "freshness")
      )
    )

    doc <- .docx_add_text_block(doc, tr(report_lang, "what_happened"), it$what_happened)
    doc <- .docx_add_text_block(doc, tr(report_lang, "why_matters"), it$why_it_matters_for_sovereign_risk)

    doc <- .docx_add_line(doc, tr(report_lang, "sources"), size = 12, bold = TRUE)
    src <- if (is.list(it)) it$sources else list()
    if (!is.list(src) || !length(src)) {
      doc <- .docx_add_line(doc, tr(report_lang, "no_links"))
    } else {
      for (s in src) {
        if (!is.list(s)) {
          next
        }
        url <- as.character(s$url)[1L]
        nm <- as.character(s$source_name)[1L]
        pd <- s$published_date
        pd_chr <- if (!is.null(pd)) as.character(pd)[1L] else ""
        if (is.na(url) || !nzchar(url)) {
          next
        }
        if (is.na(nm) || !nzchar(nm)) {
          nm <- url
        }
        prefix <- if (is.character(pd_chr) && nzchar(pd_chr) && !is.na(pd_chr)) {
          paste0(nm, " (", .docx_format_date(pd_chr), "): ")
        } else {
          paste0(nm, ": ")
        }
        source_par <- officer::fpar(
          officer::ftext(prefix, prop = .docx_text_prop()),
          officer::hyperlink_ftext(
            text = url,
            href = url,
            prop = .docx_text_prop(color = "#0563C1", underline = TRUE)
          )
        )
        doc <- officer::body_add_fpar(doc, source_par, style = "Normal")
      }
    }

    item_tag_list <- .coerce_string_list(if (is.list(it)) it$tags else NULL)
    if (length(item_tag_list)) {
      doc <- .docx_add_line(
        doc,
        paste(tr(report_lang, "tags"), paste(item_tag_list, collapse = ", "))
      )
    }

    notes <- as.character(it$notes_on_evidence_quality)[1L]
    if (!is.na(notes) && nzchar(notes)) {
      doc <- .docx_add_line(doc, paste(tr(report_lang, "evidence_quality"), notes))
    }

    doc <- .docx_add_line(doc, "")
  }

  print(doc, target = path)
  invisible(path)
}
