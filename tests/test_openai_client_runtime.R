# Runtime checks for R/openai_client.R (no API key required)
# Usage (from repo root): Rscript tests/test_openai_client_runtime.R

wd <- getwd()
if (basename(wd) == "tests") {
  setwd(dirname(wd))
}

source("R/config.R", local = FALSE)
source("R/openai_client.R", local = FALSE)

b1 <- list(
  output = list(
    list(
      type = "message",
      content = list(list(type = "output_text", text = '{"x":true}'))
    )
  )
)
stopifnot(identical(openai_responses_extract_output_text(b1), '{"x":true}'))

b2 <- jsonlite::fromJSON(
  '{"output_text":"hello"}',
  simplifyVector = FALSE
)
stopifnot(identical(openai_responses_extract_output_text(b2), "hello"))

parsed <- openai_responses_parse_json_text("{\"a\":[1,2]}")
stopifnot(identical(parsed$a, list(1L, 2L)))

fmt <- openai_responses_text_json_schema("test_schema", list(type = "object"), strict = TRUE)
stopifnot(identical(fmt$format$type, "json_schema"), isTRUE(fmt$format$strict))

stopifnot(length(openai_responses_default_web_search_tools()) == 1L)

u0 <- openai_responses_extract_usage(list(usage = list(
  input_tokens = 10L,
  output_tokens = 20L,
  total_tokens = 30L
)))
stopifnot(u0$total_tokens == 30L, nzchar(format_openai_usage_text(u0)))

u1 <- openai_responses_extract_usage(list(usage = list(prompt_tokens = 1L, completion_tokens = 2L)))
stopifnot(u1$input_tokens == 1L, u1$output_tokens == 2L)

ua <- openai_usage_add(
  list(input_tokens = 1L, output_tokens = 2L),
  list(input_tokens = 10L, output_tokens = 20L)
)
stopifnot(ua$input_tokens == 11L, ua$output_tokens == 22L, ua$total_tokens == 33L)

source("R/topics.R", local = FALSE)
source("R/prompt.R", local = FALSE)
tw <- openai_responses_news_text(n = 3L, report_language = "ENG")
stopifnot(identical(tw$format$type, "json_schema"), identical(tw$format$name, "sovereign_news_response"))
title_desc <- tw$format$schema$properties$items$items$properties$title$description
stopifnot(is.character(title_desc), grepl("English", title_desc, fixed = TRUE))

source("R/schema.R", local = FALSE)

valid_one <- list(
  country = list(iso3 = "BRA", display_name = "Brazil"),
  time_window = list(start = "2025-01-01", end = "2025-06-30"),
  topics = list("macro"),
  follow_up_queries = list(),
  items = list(
    list(
      title = "t",
      date = "2025-04-01",
      what_happened = "x",
      why_it_matters_for_sovereign_risk = "y",
      topic_category = "macro",
      scores = list(credit_importance = 3L, visibility = 3L, freshness = 3L),
      composite_rank = 1L,
      sources = list(
        list(url = "https://example.com/a", source_name = "A", published_date = ""),
        list(url = "https://example.com/b", source_name = "B", published_date = "")
      ),
      origin = "base_search",
      tags = list(),
      notes_on_evidence_quality = ""
    )
  )
)
v1 <- validate_news_response(valid_one, n_expected = 1L, report_language = "ENG")
stopifnot(isTRUE(v1$ok), length(v1$errors) == 0L)

bad <- valid_one
bad$items[[1L]]$scores$credit_importance <- 10L
v2 <- validate_news_response(bad, n_expected = 1L, report_language = "ENG")
stopifnot(!isTRUE(v2$ok), length(v2$errors) >= 1L)

bad_lang <- valid_one
bad_lang$items[[1L]]$title <- "Заголовок на русском"
v3 <- validate_news_response(bad_lang, n_expected = 1L, report_language = "ENG")
stopifnot(!isTRUE(v3$ok), any(grepl("title must be in English", v3$errors, fixed = TRUE)))

good_rus <- valid_one
good_rus$items[[1L]]$title <- "Заголовок на русском"
v4 <- validate_news_response(good_rus, n_expected = 1L, report_language = "RUS")
stopifnot(isTRUE(v4$ok), length(v4$errors) == 0L)

message("All openai_client runtime checks passed.")
