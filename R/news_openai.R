# Live OpenAI Responses pipeline for sovereign-news JSON (web search + structured output).
# Depends on: R/config.R, R/openai_client.R, R/prompt.R, R/schema.R (sourced before this file in app.R).

#' Run the full model request: system + user prompts, optional web search, strict JSON schema.
#'
#' @return `list(ok, error, parsed, usage)` where `parsed` is validated news JSON on success.
fetch_sovereign_news_response <- function(country_iso3,
                                         date_start,
                                         date_end,
                                         all_topics,
                                         topic_ids,
                                         n_news,
                                         follow_up_enabled,
                                         follow_up_queries,
                                         search_language_phrase = NULL,
                                         report_language = "RUS") {
  key <- openai_api_key()
  if (is.na(key) || !nzchar(key)) {
    return(list(
      ok = FALSE,
      error = paste0(
        "OPENAI_API_KEY is not set. Add it to your environment or a project `.Renviron` file ",
        "(restart the R session after changing)."
      ),
      parsed = NULL,
      usage = NULL
    ))
  }

  country_iso3 <- toupper(trimws(as.character(country_iso3)))
  dn <- countrycode::countrycode(country_iso3, "iso3c", "country.name.en", warn = FALSE)
  if (length(dn) != 1L || is.na(dn)) {
    dn <- country_iso3
  }

  if (is.null(search_language_phrase) || !nzchar(as.character(search_language_phrase)[1L])) {
    search_language_phrase <- "английском и основном языке (языках) страны"
  }

  n_news <- as.integer(n_news)[1L]
  if (is.na(n_news) || n_news < 1L) {
    n_news <- default_news_count()
  }
  report_language <- toupper(trimws(as.character(report_language)[1L]))
  if (!report_language %in% c("RUS", "ENG")) {
    report_language <- "RUS"
  }

  pr <- build_sovereign_news_prompt(
    country_iso3 = country_iso3,
    country_display_name = as.character(dn),
    date_start = date_start,
    date_end = date_end,
    all_topics = all_topics,
    topic_ids = topic_ids,
    n_news = n_news,
    follow_up_queries = follow_up_queries,
    follow_up_enabled = follow_up_enabled,
    search_language_phrase = search_language_phrase,
    report_language = report_language
  )

  text_fmt <- openai_responses_news_text(
    n = n_news,
    strict = openai_json_schema_strict(),
    report_language = report_language
  )

  extra_body <- list(max_output_tokens = openai_max_output_tokens())
  tools <- if (isTRUE(openai_web_search_enabled())) {
    openai_responses_default_web_search_tools(type = openai_web_search_tool_type())
  } else {
    NULL
  }

  tool_choice <- if (!is.null(tools)) "auto" else NULL

  res <- openai_responses_create(
    instructions = pr$system,
    input = pr$user,
    model = NULL,
    text = text_fmt,
    tools = tools,
    tool_choice = tool_choice,
    parse_json = TRUE,
    json_repair_attempt = TRUE,
    validate_parsed_json = function(parsed) {
      validate_news_response(
        parsed,
        n_expected = n_news,
        report_language = report_language
      )
    },
    extra_body = extra_body
  )

  if (!isTRUE(res$ok)) {
    err <- res$error
    if (is.null(err) || !nzchar(as.character(err))) {
      err <- sprintf("OpenAI error (HTTP %s)", res$status %||% "?")
    }
    return(list(ok = FALSE, error = err, parsed = res$parsed_json, usage = res$usage))
  }

  list(ok = TRUE, error = NULL, parsed = res$parsed_json, usage = res$usage)
}
