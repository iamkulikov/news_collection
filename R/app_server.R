# Server: wiring for baseline inputs/outputs (API integration comes later)

app_server <- function(input, output, session) {
  observeEvent(input$btn_last_6m, {
    dr <- default_date_range()
    updateDateRangeInput(
      session,
      "date_range",
      start = dr[["start"]],
      end = dr[["end"]]
    )
  })

  observeEvent(input$topics_select_all, {
    req(isFALSE(input$all_topics))
    updateCheckboxGroupInput(session, "topics", selected = all_topic_ids)
  })

  observeEvent(input$topics_clear, {
    req(isFALSE(input$all_topics))
    updateCheckboxGroupInput(session, "topics", selected = character())
  })

  status <- reactiveVal(
    "Set the country, period, and options on the left, then click “Find news”. Results appear here after a successful run."
  )

  last_validated_response <- reactiveVal(NULL)

  observeEvent(input$run, {
    withProgress(message = "Finding economic news…", value = 0, {
      setProgress(0.08, detail = "Checking inputs")
      country_iso3 <- input$country
      if (is.null(country_iso3) || !nzchar(country_iso3)) {
        country_iso3 <- ""
      }
      start <- input$date_range[1]
      end <- input$date_range[2]
      topics_sel <- if (isTRUE(input$all_topics)) all_topic_ids else input$topics
      follow <- process_followup_input(
        isTRUE(input$followup_enabled),
        input$followup_topics
      )

      if (identical(country_iso3, "")) {
        status("Choose a country in the sidebar (search by name or ISO2/ISO3 code).")
        last_validated_response(NULL)
        setProgress(1, detail = "Stopped — country missing")
        return()
      }

      if (!country_iso3_valid(country_iso3)) {
        status(
          "That country value is not recognized. Pick an entry from the dropdown so the ISO code is valid."
        )
        last_validated_response(NULL)
        setProgress(1, detail = "Stopped — invalid country")
        return()
      }

      pv <- validate_period_span(start, end)
      if (!isTRUE(pv$ok)) {
        status(paste(pv$errors, collapse = "\n"))
        last_validated_response(NULL)
        setProgress(1, detail = "Stopped — check the period")
        return()
      }

      nc <- validate_news_count_input(input$news_count)
      if (!isTRUE(nc$ok)) {
        status(paste(nc$errors, collapse = "\n"))
        last_validated_response(NULL)
        setProgress(1, detail = "Stopped — check N")
        return()
      }

      tv <- validate_topic_selection(input$all_topics, topics_sel)
      if (!isTRUE(tv$ok)) {
        status(paste(tv$errors, collapse = "\n"))
        last_validated_response(NULL)
        setProgress(1, detail = "Stopped — check topics")
        return()
      }

      if (!follow$ok) {
        status(paste(c("Follow-up topics:", follow$errors), collapse = "\n"))
        last_validated_response(NULL)
        setProgress(1, detail = "Stopped — check follow-up")
        return()
      }

      fu_topics <- if (isTRUE(input$followup_enabled)) follow$topics else character()
      cache_key <- news_request_cache_key(
        country_iso3,
        start,
        end,
        topics_sel,
        fu_topics,
        nc$n,
        openai_default_model()
      )
      cached <- news_cache_get(cache_key)
      if (!is.null(cached)) {
        setProgress(0.55, detail = "Loading cached results…")
        last_validated_response(cached$parsed)
        uline <- format_openai_usage_text(cached$usage)
        if (!nzchar(uline)) uline <- "(no usage stored)"
        status(paste0(
          "Loaded from cache (same request as within the last ",
          round(news_cache_ttl_sec() / 3600, 1),
          " h). Cache key starts with: ",
          substr(cache_key, 1L, 12L),
          "…\n",
          "Saved at: ", format(cached$saved_at, usetz = TRUE), "\n",
          "API usage (stored): ", uline, "\n",
          "Run summary — country: ", country_iso3,
          " | period: ", start, " → ", end,
          " | N: ", nc$n,
          " | topics: ", if (length(topics_sel)) paste(topics_sel, collapse = ", ") else "(none)",
          if (length(fu_topics)) paste0(          " | follow-up: ", length(fu_topics), " line(s)") else "",
          "\n",
          "Cached response — same parameters as a previous run within the TTL."
        ))
        setProgress(1, detail = "Loaded from cache")
        return()
      }

      if (!openai_has_api_key()) {
        status(paste0(
          "OPENAI_API_KEY is not set. Add your API key to `.Renviron` or the environment, ",
          "restart R / the Shiny app, then try again."
        ))
        last_validated_response(NULL)
        setProgress(1, detail = "Stopped — no API key")
        return()
      }

      setProgress(0.22, detail = "Calling OpenAI (web search + JSON)…")
      api <- fetch_sovereign_news_response(
        country_iso3 = country_iso3,
        date_start = start,
        date_end = end,
        all_topics = isTRUE(input$all_topics),
        topic_ids = topics_sel,
        n_news = nc$n,
        follow_up_enabled = isTRUE(input$followup_enabled),
        follow_up_queries = fu_topics
      )

      if (!isTRUE(api$ok)) {
        status(paste0(
          "OpenAI request failed:\n",
          api$error,
          "\n\n",
          "If you see 403, try setting OPENAI_WEB_SEARCH=false or use a model with tool access. ",
          "If 429, wait and retry or reduce N / shorten the period."
        ))
        last_validated_response(NULL)
        setProgress(1, detail = "Stopped — API error")
        return()
      }

      setProgress(0.82, detail = "Validating response…")
      parsed <- api$parsed
      chk <- validate_news_response(parsed, n_expected = nc$n)
      if (!isTRUE(chk$ok)) {
        status(paste(
          c(
            "The API returned JSON that failed validation (unexpected):",
            chk$errors
          ),
          collapse = "\n"
        ))
        last_validated_response(NULL)
        setProgress(1, detail = "Stopped — validation error")
        return()
      }

      last_validated_response(parsed)
      news_cache_set(cache_key, parsed, usage = api$usage)

      fu_line <- if (length(fu_topics)) {
        paste0(" | follow-up: ", length(fu_topics), " line(s)")
      } else {
        ""
      }
      uline <- format_openai_usage_text(api$usage)
      if (!nzchar(uline)) uline <- "(not reported)"

      n_items <- if (is.list(parsed$items)) length(parsed$items) else 0L
      status(paste0(
        "Finished at ", format(Sys.time(), usetz = TRUE),
        ". Result cached for ", round(news_cache_ttl_sec() / 3600, 1), " h.\n",
        "Run summary — country: ", country_iso3,
        " | period: ", start, " → ", end,
        " | N: ", nc$n,
        " | items returned: ", n_items,
        " | topics: ", if (length(topics_sel)) paste(topics_sel, collapse = ", ") else "(none)",
        fu_line,
        "\nAPI usage: ", uline
      ))

      setProgress(1, detail = "Done")
    })
  })

  output$status <- renderText({
    status()
  })

  output$results <- renderUI({
    render_news_response_ui(last_validated_response())
  })

  output$export_json <- downloadHandler(
    filename = function() {
      paste0("sovereign_news_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
    },
    content = function(file) {
      p <- last_validated_response()
      req(!is.null(p))
      jsonlite::write_json(p, file, auto_unbox = TRUE, pretty = TRUE)
    }
  )

  output$export_csv <- downloadHandler(
    filename = function() {
      paste0("sovereign_news_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      p <- last_validated_response()
      req(!is.null(p))
      df <- news_response_to_csv_df(p)
      utils::write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$export_docx <- downloadHandler(
    filename = function() {
      paste0("sovereign_news_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx")
    },
    content = function(file) {
      p <- last_validated_response()
      req(!is.null(p))
      write_news_response_docx(p, file)
    }
  )
}
