# Runtime checks for DOCX export formatting rules.
# Usage (from repo root): Rscript tests/test_docx_formatting_runtime.R

wd <- getwd()
if (basename(wd) == "tests") {
  setwd(dirname(wd))
}

source("R/topics.R", local = FALSE)
source("R/i18n.R", local = FALSE)
source("R/render.R", local = FALSE)

parsed <- list(
  country = list(iso3 = "BRA", display_name = "Brazil"),
  time_window = list(start = "2025-10-01", end = "2026-03-31"),
  topics = list("macro"),
  report_language = "ENG",
  follow_up_queries = list(),
  items = list(
    list(
      title = "Sovereign spread widens after budget revision",
      date = "2026-03-10",
      what_happened = "Bond spreads moved up after revised fiscal projections.",
      why_it_matters_for_sovereign_risk = "Higher refinancing costs can weaken debt dynamics.",
      topic_category = "budget",
      scores = list(credit_importance = 5L, visibility = 4L, freshness = 5L),
      composite_rank = 1L,
      sources = list(
        list(
          url = "https://example.com/article-a",
          source_name = "Source A",
          published_date = "2026-03-10"
        )
      ),
      origin = "base_search",
      tags = list("fiscal"),
      notes_on_evidence_quality = "Confirmed by multiple outlets."
    )
  )
)

tmp_docx <- tempfile(fileext = ".docx")
write_news_response_docx(parsed, tmp_docx, report_lang = "ENG")

extract_dir <- tempfile("docx_extract_")
dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
utils::unzip(tmp_docx, files = c("word/document.xml", "word/_rels/document.xml.rels"), exdir = extract_dir)

doc_xml <- paste(readLines(file.path(extract_dir, "word", "document.xml"), warn = FALSE), collapse = "\n")
rels_xml <- paste(readLines(file.path(extract_dir, "word", "_rels", "document.xml.rels"), warn = FALSE), collapse = "\n")

stopifnot(grepl("Brazil Economic News - 01.10.2025 to 31.03.2026", doc_xml, fixed = TRUE))
stopifnot(!grepl("Country Risk News Finder", doc_xml, fixed = TRUE))
stopifnot(grepl("1. Sovereign spread widens after budget revision", doc_xml, fixed = TRUE))
stopifnot(grepl("<w:hyperlink", doc_xml, fixed = TRUE))
stopifnot(grepl("https://example.com/article-a", rels_xml, fixed = TRUE))
stopifnot(grepl("Calibri", doc_xml, fixed = TRUE))

message("All DOCX formatting runtime checks passed.")
