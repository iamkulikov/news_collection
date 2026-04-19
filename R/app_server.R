# Server: wiring for baseline inputs/outputs (API integration comes later)

app_server <- function(input, output, session) {
  show_search_progress_modal <- function(report_lang) {
    showModal(
      modalDialog(
        title = tr(report_lang, "results_progress_title"),
        easyClose = FALSE,
        footer = NULL,
        size = "m",
        tags$div(
          class = "d-flex align-items-center gap-3",
          tags$div(
            class = "spinner-border text-primary",
            role = "status",
            `aria-label` = tr(report_lang, "results_progress_title")
          ),
          tags$p(class = "mb-0", tr(report_lang, "results_progress_hint"))
        )
      )
    )
  }

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
    tr("RUS", "status_initial")
  )
  run_in_progress <- reactiveVal(FALSE)
  progress_value <- reactiveVal(0)
  progress_detail <- reactiveVal("")

  last_validated_response <- reactiveVal(NULL)
  last_report_lang <- reactiveVal("RUS")
  current_report_lang <- reactive({
    normalize_report_language(input$report_lang)
  })

  observeEvent(input$run, {
    report_lang <- normalize_report_language(input$report_lang)
    run_in_progress(TRUE)
    progress_value(0.05)
    progress_detail("Checking inputs…")
    show_search_progress_modal(report_lang)
    withProgress(message = "Finding economic news...", value = 0, {
      on.exit({
        run_in_progress(FALSE)
        removeModal()
      }, add = TRUE)

      setProgress(0.08, detail = "Checking inputs")
      progress_value(0.08)
      progress_detail("Checking inputs")
      country_iso3 <- input$country
      if (is.null(country_iso3) || !nzchar(country_iso3)) {
        country_iso3 <- ""
      }
      dr_input <- input$date_range
      start <- parse_ui_date(dr_input[1])
      end <- parse_ui_date(dr_input[2])
      start_iso <- as.character(start)
      end_iso <- as.character(end)
      start_ui <- format_ui_date(start)
      end_ui <- format_ui_date(end)
      topics_sel <- if (isTRUE(input$all_topics)) all_topic_ids else input$topics
      follow <- process_followup_input(
        isTRUE(input$followup_enabled),
        input$followup_topics
      )

      if (identical(country_iso3, "")) {
        status(tr(report_lang, "status_country_missing"))
        last_validated_response(NULL)
        setProgress(1, detail = "Stopped — country missing")
        progress_value(1)
        progress_detail("Stopped — country missing")
        return()
      }

      if (!country_iso3_valid(country_iso3)) {
        status(tr(report_lang, "status_country_invalid"))
        last_validated_response(NULL)
        setProgress(1, detail = "Stopped — invalid country")
        progress_value(1)
        progress_detail("Stopped — invalid country")
        return()
      }

      pv <- validate_period_span(start, end)
      if (!isTRUE(pv$ok)) {
        status(paste(pv$errors, collapse = "\n"))
        last_validated_response(NULL)
        setProgress(1, detail = "Stopped — check the period")
        progress_value(1)
        progress_detail("Stopped — check the period")
        return()
      }

      nc <- validate_news_count_input(input$news_count)
      if (!isTRUE(nc$ok)) {
        status(paste(nc$errors, collapse = "\n"))
        last_validated_response(NULL)
        setProgress(1, detail = "Stopped — check N")
        progress_value(1)
        progress_detail("Stopped — check N")
        return()
      }

      tv <- validate_topic_selection(input$all_topics, topics_sel)
      if (!isTRUE(tv$ok)) {
        status(paste(tv$errors, collapse = "\n"))
        last_validated_response(NULL)
        setProgress(1, detail = "Stopped — check topics")
        progress_value(1)
        progress_detail("Stopped — check topics")
        return()
      }

      if (!follow$ok) {
        status(paste(c(tr(report_lang, "status_followup_prefix"), follow$errors), collapse = "\n"))
        last_validated_response(NULL)
        setProgress(1, detail = "Stopped — check follow-up")
        progress_value(1)
        progress_detail("Stopped — check follow-up")
        return()
      }

      fu_topics <- if (isTRUE(input$followup_enabled)) follow$topics else character()
      cache_key <- news_request_cache_key(
        country_iso3,
        start_iso,
        end_iso,
        topics_sel,
        fu_topics,
        nc$n,
        openai_default_model(),
        report_lang
      )
      cached <- news_cache_get(cache_key)
      if (!is.null(cached)) {
        cached_chk <- validate_news_response(
          cached$parsed,
          n_expected = nc$n,
          report_language = report_lang
        )
        if (!isTRUE(cached_chk$ok)) {
          cached <- NULL
        }
      }
      if (!is.null(cached)) {
        setProgress(0.55, detail = "Loading cached results…")
        progress_value(0.55)
        progress_detail("Loading cached results…")
        last_validated_response(cached$parsed)
        last_report_lang(report_lang)
        uline <- format_openai_usage_text(cached$usage)
        if (!nzchar(uline)) uline <- "(not reported)"
        fu_line <- if (length(fu_topics)) {
          paste0(" | ", tr(report_lang, "follow_up"), ": ", length(fu_topics), " line(s)")
        } else {
          ""
        }
        topics_line <- if (length(topics_sel)) paste(topics_sel, collapse = ", ") else "(none)"
        status(tr(
          report_lang,
          "status_cache_loaded",
          round(news_cache_ttl_sec() / 3600, 1),
          substr(cache_key, 1L, 12L),
          format(cached$saved_at, usetz = TRUE),
          uline,
          country_iso3,
          start_ui,
          end_ui,
          nc$n,
          report_lang,
          topics_line,
          fu_line
        ))
        setProgress(1, detail = "Loaded from cache")
        progress_value(1)
        progress_detail("Loaded from cache")
        return()
      }

      if (!openai_has_api_key()) {
        status(tr(report_lang, "status_api_key_missing"))
        last_validated_response(NULL)
        setProgress(1, detail = "Stopped — no API key")
        progress_value(1)
        progress_detail("Stopped — no API key")
        return()
      }

      setProgress(0.22, detail = "Calling OpenAI (web search + JSON)…")
      progress_value(0.22)
      progress_detail("Calling OpenAI (web search + JSON)…")
      api <- fetch_sovereign_news_response(
        country_iso3 = country_iso3,
        date_start = start_iso,
        date_end = end_iso,
        all_topics = isTRUE(input$all_topics),
        topic_ids = topics_sel,
        n_news = nc$n,
        follow_up_enabled = isTRUE(input$followup_enabled),
        follow_up_queries = fu_topics,
        report_language = report_lang
      )

      if (!isTRUE(api$ok)) {
        status(tr(report_lang, "status_api_failed", api$error))
        last_validated_response(NULL)
        setProgress(1, detail = "Stopped — API error")
        progress_value(1)
        progress_detail("Stopped — API error")
        return()
      }

      setProgress(0.82, detail = "Validating response…")
      progress_value(0.82)
      progress_detail("Validating response…")
      parsed <- api$parsed
      chk <- validate_news_response(
        parsed,
        n_expected = nc$n,
        report_language = report_lang
      )
      if (!isTRUE(chk$ok)) {
        status(paste(
          c(
            tr(report_lang, "status_validation_failed"),
            chk$errors
          ),
          collapse = "\n"
        ))
        last_validated_response(NULL)
        setProgress(1, detail = "Stopped — validation error")
        progress_value(1)
        progress_detail("Stopped — validation error")
        return()
      }

      last_validated_response(parsed)
      last_report_lang(report_lang)
      news_cache_set(cache_key, parsed, usage = api$usage)

      fu_line <- if (length(fu_topics)) {
        paste0(" | ", tr(report_lang, "follow_up"), ": ", length(fu_topics), " line(s)")
      } else {
        ""
      }
      uline <- format_openai_usage_text(api$usage)
      if (!nzchar(uline)) uline <- "(not reported)"

      n_items <- if (is.list(parsed$items)) length(parsed$items) else 0L
      topics_line <- if (length(topics_sel)) paste(topics_sel, collapse = ", ") else "(none)"
      status(tr(
        report_lang,
        "status_finished",
        format(Sys.time(), usetz = TRUE),
        round(news_cache_ttl_sec() / 3600, 1),
        country_iso3,
        start_ui,
        end_ui,
        nc$n,
        n_items,
        report_lang,
        topics_line,
        fu_line,
        uline
      ))

      setProgress(1, detail = "Done")
      progress_value(1)
      progress_detail("Done")
    })
  })

  output$status <- renderText({
    status()
  })

  observeEvent(current_report_lang(), {
    if (is.null(last_validated_response())) {
      status(tr(current_report_lang(), "status_initial"))
    }
  }, ignoreInit = TRUE)

  output$status_header <- renderText({
    tr(current_report_lang(), "status_header")
  })

  output$results_header <- renderText({
    tr(current_report_lang(), "results_header")
  })

  output$export_header <- renderText({
    tr(current_report_lang(), "export_header")
  })

  output$export_controls <- renderUI({
    rl <- current_report_lang()
    tagList(
      p(class = "text-muted small", tr(rl, "export_help")),
      div(
        class = "export-actions-sticky d-grid gap-2",
        downloadButton("export_json", tr(rl, "export_json"), class = "btn-outline-secondary w-100"),
        downloadButton("export_csv", tr(rl, "export_csv"), class = "btn-outline-secondary w-100"),
        downloadButton("export_docx", tr(rl, "export_docx"), class = "btn-outline-secondary w-100")
      )
    )
  })

  output$results <- renderUI({
    rl <- current_report_lang()
    if (isTRUE(run_in_progress())) {
      pct <- as.integer(round(100 * max(0, min(1, progress_value()))))
      return(
        tags$div(
          class = "alert alert-primary border shadow-sm",
          tags$h5(class = "mb-2", tr(rl, "results_progress_title")),
          tags$p(class = "mb-3", tr(rl, "results_progress_hint")),
          tags$div(
            class = "progress mb-2",
            tags$div(
              class = "progress-bar progress-bar-striped progress-bar-animated",
              role = "progressbar",
              style = paste0("width: ", pct, "%"),
              `aria-valuenow` = pct,
              `aria-valuemin` = "0",
              `aria-valuemax` = "100"
            )
          ),
          tags$div(class = "small text-muted", progress_detail())
        )
      )
    }

    parsed <- last_validated_response()
    lang_for_render <- if (is.null(parsed)) rl else last_report_lang()
    render_news_response_ui(
      parsed,
      report_lang = lang_for_render
    )
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
      write_news_response_docx(
        p,
        file,
        report_lang = last_report_lang()
      )
    }
  )
}
