# Runtime UX regression checks for language switching and layout stability.
# Usage (from repo root): Rscript tests/test_ux_regression_runtime.R

wd <- getwd()
if (basename(wd) == "tests") {
  setwd(dirname(wd))
}

suppressPackageStartupMessages({
  library(shiny)
  library(htmltools)
  library(bslib)
})

source("R/countries.R", local = FALSE)
source("R/topics.R", local = FALSE)
source("R/config.R", local = FALSE)
source("R/cache.R", local = FALSE)
source("R/openai_client.R", local = FALSE)
source("R/prompt.R", local = FALSE)
source("R/schema.R", local = FALSE)
source("R/news_openai.R", local = FALSE)
source("R/i18n.R", local = FALSE)
source("R/render.R", local = FALSE)
source("R/followup.R", local = FALSE)
source("R/app_ui.R", local = FALSE)
source("R/app_server.R", local = FALSE)

sample_item <- function(rank = 1L, what = "What happened", why = "Why it matters") {
  list(
    title = sprintf("News %s", rank),
    date = "2026-03-10",
    what_happened = what,
    why_it_matters_for_sovereign_risk = why,
    topic_category = "macro",
    scores = list(credit_importance = 4L, visibility = 3L, freshness = 5L),
    composite_rank = rank,
    sources = list(
      list(url = "https://example.com/a", source_name = "Source A", published_date = "2026-03-10"),
      list(url = "https://example.com/b", source_name = "Source B", published_date = "2026-03-09")
    ),
    origin = "base_search",
    tags = list("macro", "policy"),
    notes_on_evidence_quality = "Multi-source confirmation."
  )
}

sample_response <- function(items) {
  list(
    country = list(iso3 = "BRA", display_name = "Brazil"),
    time_window = list(start = "2025-10-01", end = "2026-03-31"),
    topics = list("macro"),
    report_language = "RUS",
    follow_up_queries = list(),
    items = items
  )
}

ui_source <- paste(readLines("R/app_ui.R", warn = FALSE), collapse = "\n")

stopifnot(grepl("\\.results-panel, \\.export-panel \\{ min-height: 32rem; \\}", ui_source))
stopifnot(grepl("\\.status-scroll \\{ max-height: 14rem; overflow-y: auto; \\}", ui_source))
stopifnot(grepl("\\.results-scroll \\{ min-height: 26rem; max-height: 70vh; overflow-y: auto; \\}", ui_source))
stopifnot(grepl("\\.export-scroll \\{ max-height: 70vh; overflow-y: auto; \\}", ui_source))
stopifnot(grepl("id = \"export_panel_accordion\"", ui_source, fixed = TRUE))
stopifnot(grepl("open = NULL", ui_source, fixed = TRUE))

long_line <- paste(rep("long content", 80), collapse = " ")
long_items <- lapply(1:20, function(i) {
  sample_item(
    rank = i,
    what = paste(long_line, sprintf("(what %s)", i)),
    why = paste(long_line, sprintf("(why %s)", i))
  )
})
long_response <- sample_response(long_items)
long_results_html <- renderTags(render_news_response_ui(long_response, report_lang = "ENG"))$html
stopifnot(length(gregexpr("mb-3 border shadow-sm", long_results_html, fixed = TRUE)[[1]]) == 20L)
stopifnot(grepl("What happened", long_results_html, fixed = TRUE))
stopifnot(grepl("Why it matters for sovereign risk", long_results_html, fixed = TRUE))

rus_results_html <- renderTags(render_news_response_ui(sample_response(list(sample_item())), report_lang = "RUS"))$html
eng_results_html <- renderTags(render_news_response_ui(sample_response(list(sample_item())), report_lang = "ENG"))$html
stopifnot(grepl("Что произошло", rus_results_html, fixed = TRUE))
stopifnot(grepl("Почему это важно для суверенного риска", rus_results_html, fixed = TRUE))
stopifnot(grepl("What happened", eng_results_html, fixed = TRUE))
stopifnot(grepl("Why it matters for sovereign risk", eng_results_html, fixed = TRUE))

testServer(app_server, {
  stopifnot(identical(output$status_header, tr("RUS", "status_header")))
  stopifnot(identical(output$results_header, tr("RUS", "results_header")))
  stopifnot(grepl("Задайте страну", output$status, fixed = TRUE))

  session$setInputs(report_lang = "ENG")
  stopifnot(identical(output$status_header, tr("ENG", "status_header")))
  stopifnot(identical(output$results_header, tr("ENG", "results_header")))
  stopifnot(grepl("Set the country", output$status, fixed = TRUE))

  session$setInputs(report_lang = "RUS")
  stopifnot(identical(output$export_header, tr("RUS", "export_header")))
  stopifnot(grepl("Задайте страну", output$status, fixed = TRUE))
})

message("All UX regression runtime checks passed.")
