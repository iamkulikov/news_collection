# Server: wiring for baseline inputs/outputs (API integration comes later)

progress_stage_label <- function(report_lang, stage_id) {
  key <- switch(
    as.character(stage_id)[1L],
    checking_inputs = "progress_stage_checking_inputs",
    loading_cache = "progress_stage_loading_cache",
    calling_openai = "progress_stage_calling_openai",
    waiting_model = "progress_stage_waiting_model",
    repairing_json = "progress_stage_repairing_json",
    validating_response = "progress_stage_validating_response",
    done = "progress_stage_done",
    stopped = "progress_stage_stopped",
    "results_progress_title"
  )
  tr(report_lang, key)
}

format_run_elapsed <- function(started_at, now = Sys.time()) {
  format_elapsed_seconds <- function(secs) {
    if (!is.finite(secs) || secs < 0) {
      secs <- 0
    }
    secs <- as.integer(round(secs))
    hrs <- secs %/% 3600L
    mins <- (secs %% 3600L) %/% 60L
    rem_secs <- secs %% 60L
    if (hrs >= 1L) {
      sprintf("%02d:%02d:%02d", hrs, mins, rem_secs)
    } else {
      sprintf("%02d:%02d", mins, rem_secs)
    }
  }
  if (is.null(started_at) || length(started_at) != 1L || is.na(started_at)) {
    return(format_elapsed_seconds(0))
  }
  secs <- suppressWarnings(as.numeric(difftime(now, started_at, units = "secs")))
  format_elapsed_seconds(secs)
}

format_duration_seconds <- function(seconds) {
  if (is.null(seconds) || length(seconds) != 1L || is.na(seconds)) {
    return("00:00")
  }
  secs <- suppressWarnings(as.numeric(seconds))
  if (!is.finite(secs) || secs < 0) {
    secs <- 0
  }
  secs <- as.integer(round(secs))
  hrs <- secs %/% 3600L
  mins <- (secs %% 3600L) %/% 60L
  rem_secs <- secs %% 60L
  if (hrs >= 1L) {
    sprintf("%02d:%02d:%02d", hrs, mins, rem_secs)
  } else {
    sprintf("%02d:%02d", mins, rem_secs)
  }
}

ensure_progress_future_plan <- local({
  configured <- FALSE
  function() {
    if (isTRUE(configured)) {
      return(invisible(TRUE))
    }
    future::plan(future::multisession)
    configured <<- TRUE
    invisible(TRUE)
  }
})

progress_is_terminal_stage <- function(stage_id) {
  as.character(stage_id)[1L] %in% c("done", "stopped")
}

progress_started_at_ms <- function(started_at) {
  if (is.null(started_at) || length(started_at) != 1L || is.na(started_at)) {
    return("0")
  }
  sprintf("%.0f", as.numeric(started_at) * 1000)
}

progress_value_from_eta <- function(started_at,
                                    estimated_seconds,
                                    stage_id = NULL,
                                    now = Sys.time()) {
  if (progress_is_terminal_stage(stage_id)) {
    return(1)
  }
  if (is.null(started_at) || length(started_at) != 1L || is.na(started_at)) {
    return(0)
  }
  eta_sec <- suppressWarnings(as.numeric(estimated_seconds)[1L])
  if (!is.finite(eta_sec) || eta_sec <= 0) {
    return(0)
  }
  elapsed_sec <- suppressWarnings(as.numeric(difftime(now, started_at, units = "secs")))
  if (!is.finite(elapsed_sec) || elapsed_sec < 0) {
    elapsed_sec <- 0
  }
  pct <- min(elapsed_sec / eta_sec, 0.97)
  max(pct, 0.05)
}

format_progress_eta_text <- function(report_lang,
                                     started_at,
                                     estimated_seconds,
                                     stage_id = NULL,
                                     now = Sys.time()) {
  if (progress_is_terminal_stage(stage_id)) {
    return(tr(report_lang, "progress_eta_done"))
  }
  eta_sec <- suppressWarnings(as.numeric(estimated_seconds)[1L])
  if (!is.finite(eta_sec) || eta_sec <= 0) {
    return("—")
  }
  elapsed_sec <- suppressWarnings(as.numeric(difftime(now, started_at, units = "secs")))
  if (!is.finite(elapsed_sec) || elapsed_sec < 0) {
    elapsed_sec <- 0
  }
  if (elapsed_sec <= eta_sec) {
    return(tr(
      report_lang,
      "progress_eta_remaining_template",
      format_duration_seconds(eta_sec - elapsed_sec)
    ))
  }
  tr(
    report_lang,
    "progress_eta_overdue_template",
    format_duration_seconds(elapsed_sec - eta_sec)
  )
}

progress_failure_label <- function(report_lang, failure_kind = NULL) {
  fk <- as.character(failure_kind)[1L]
  if (is.na(fk) || !nzchar(fk)) {
    return(character())
  }
  key <- switch(
    fk,
    timeout = "progress_failure_timeout",
    rate_limit = "progress_failure_rate_limit",
    access = "progress_failure_access",
    network = "progress_failure_network",
    repair_failed = "progress_failure_repair_failed",
    validation = "progress_failure_validation",
    generic = "progress_failure_generic",
    NULL
  )
  if (is.null(key)) {
    return(character())
  }
  tr(report_lang, key)
}

progress_tech_notes <- function(report_lang, cache_state = NULL, repair_state = NULL, failure_kind = NULL) {
  notes <- character()
  if (identical(cache_state, "hit")) {
    notes <- c(notes, tr(report_lang, "progress_cache_hit"))
  } else if (identical(cache_state, "miss")) {
    notes <- c(notes, tr(report_lang, "progress_cache_miss"))
  }
  if (identical(repair_state, "in_progress")) {
    notes <- c(notes, tr(report_lang, "progress_repair_in_progress"))
  } else if (identical(repair_state, "attempted")) {
    notes <- c(notes, tr(report_lang, "progress_repair_attempted"))
  }
  notes <- c(notes, progress_failure_label(report_lang, failure_kind = failure_kind))
  unique(notes)
}

format_progress_tech_summary <- function(report_lang, cache_state = NULL, repair_state = NULL, failure_kind = NULL) {
  notes <- progress_tech_notes(
    report_lang = report_lang,
    cache_state = cache_state,
    repair_state = repair_state,
    failure_kind = failure_kind
  )
  if (length(notes)) {
    paste(notes, collapse = " | ")
  } else {
    "—"
  }
}

progress_bar_style <- function(pct, stage_id = NULL) {
  pct_num <- suppressWarnings(as.numeric(pct)[1L])
  if (!is.finite(pct_num) || pct_num < 0) {
    pct_num <- 0
  }
  pct_num <- min(pct_num, 100)
  min_width <- if (pct_num > 0 && pct_num < 4 && !progress_is_terminal_stage(stage_id)) {
    "0.9rem"
  } else {
    "0"
  }
  paste0("width: ", pct_num, "%; min-width: ", min_width)
}

build_search_progress_body_ui <- function(report_lang,
                                          stage_id,
                                          started_at,
                                          estimated_seconds,
                                          model_label,
                                          cache_state = NULL,
                                          repair_state = NULL,
                                          failure_kind = NULL) {
  pct <- as.integer(round(100 * progress_value_from_eta(
    started_at = started_at,
    estimated_seconds = estimated_seconds,
    stage_id = stage_id
  )))
  elapsed_text <- format_run_elapsed(started_at)
  eta_text <- format_progress_eta_text(
    report_lang = report_lang,
    started_at = started_at,
    estimated_seconds = estimated_seconds,
    stage_id = stage_id
  )
  tags$div(
    class = "d-flex flex-column gap-2 search-progress-widget",
    `data-started-at-ms` = progress_started_at_ms(started_at),
    `data-estimate-seconds` = sprintf("%.1f", suppressWarnings(as.numeric(estimated_seconds)[1L])),
    `data-terminal` = if (progress_is_terminal_stage(stage_id)) "true" else "false",
    `data-eta-remaining-template` = tr(report_lang, "progress_eta_remaining_template", "%s"),
    `data-eta-overdue-template` = tr(report_lang, "progress_eta_overdue_template", "%s"),
    `data-eta-done-label` = tr(report_lang, "progress_eta_done"),
    tags$div(class = "small text-muted", model_label),
    tags$div(
      class = "small",
      tags$strong(paste0(tr(report_lang, "progress_current_step"), ": ")),
      tags$span(class = "js-progress-stage", progress_stage_label(report_lang, stage_id))
    ),
    tags$div(
      class = "progress",
      tags$div(
        class = "progress-bar progress-bar-striped progress-bar-animated js-eta-progress-bar",
        role = "progressbar",
        style = progress_bar_style(pct, stage_id = stage_id),
        `aria-valuenow` = pct,
        `aria-valuemin` = "0",
        `aria-valuemax` = "100",
        tags$span(class = "visually-hidden", paste0(pct, "%"))
      )
    ),
    tags$div(
      class = "small text-muted",
      tags$strong("Progress: "),
      tags$span(class = "js-progress-pct", paste0(pct, "%"))
    ),
    tags$div(
      class = "small text-muted",
      tags$strong(paste0(tr(report_lang, "progress_elapsed"), ": ")),
      tags$span(class = "js-progress-elapsed", elapsed_text)
    ),
    tags$div(
      class = "small text-muted",
      tags$strong(paste0(tr(report_lang, "progress_eta"), ": ")),
      tags$span(class = "js-progress-eta", eta_text)
    )
  )
}

failure_title_key <- function(failure_kind = NULL) {
  fk <- as.character(failure_kind)[1L]
  if (is.na(fk) || !nzchar(fk)) {
    fk <- "generic"
  }
  switch(
    fk,
    timeout = "status_failure_title_timeout",
    rate_limit = "status_failure_title_rate_limit",
    access = "status_failure_title_access",
    network = "status_failure_title_network",
    repair_failed = "status_failure_title_repair_failed",
    validation = "status_failure_title_validation",
    generic = "status_failure_title_generic",
    "status_failure_title_generic"
  )
}

failure_hint_key <- function(failure_kind = NULL) {
  fk <- as.character(failure_kind)[1L]
  if (is.na(fk) || !nzchar(fk)) {
    fk <- "generic"
  }
  switch(
    fk,
    timeout = "status_failure_hint_timeout",
    rate_limit = "status_failure_hint_rate_limit",
    access = "status_failure_hint_access",
    network = "status_failure_hint_network",
    repair_failed = "status_failure_hint_repair_failed",
    validation = "status_failure_hint_validation",
    generic = "status_failure_hint_generic",
    "status_failure_hint_generic"
  )
}

build_failure_status_message <- function(report_lang,
                                         failure_kind = NULL,
                                         error_message,
                                         elapsed_text,
                                         cache_state = NULL,
                                         repair_state = NULL) {
  bits <- c(
    tr(report_lang, failure_title_key(failure_kind)),
    tr(
      report_lang,
      "status_failure_summary",
      elapsed_text,
      format_progress_tech_summary(
        report_lang,
        cache_state = cache_state,
        repair_state = repair_state,
        failure_kind = failure_kind
      )
    )
  )
  err <- as.character(error_message)[1L]
  if (!is.na(err) && nzchar(err)) {
    bits <- c(bits, err)
  }
  bits <- c(bits, tr(report_lang, failure_hint_key(failure_kind)))
  paste(bits, collapse = "\n")
}

app_server <- function(input, output, session) {
  try(ensure_progress_future_plan(), silent = TRUE)

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
  progress_stage <- reactiveVal("checking_inputs")
  progress_started_at <- reactiveVal(as.POSIXct(NA))
  progress_estimated_sec <- reactiveVal(estimate_run_duration_sec())
  progress_cache_state <- reactiveVal(NA_character_)
  progress_repair_state <- reactiveVal(NA_character_)
  progress_failure_kind <- reactiveVal(NA_character_)
  progress_run_lang <- reactiveVal("RUS")
  # Server-driven ETA snapshots every second (Variant C).
  progress_tick <- reactiveTimer(1000)

  last_validated_response <- reactiveVal(NULL)
  last_report_lang <- reactiveVal("RUS")
  current_report_lang <- reactive({
    normalize_report_language(input$report_lang)
  })

  build_search_progress_modal <- function() {
    rl <- progress_run_lang()
    modalDialog(
      title = tr(rl, "results_progress_title"),
      easyClose = FALSE,
      footer = NULL,
      size = "m",
      class = "search-progress-modal",
      uiOutput("search_progress_modal_body")
    )
  }

  show_progress_modal_now <- function() {
    if (!isTRUE(run_in_progress())) {
      return(invisible(NULL))
    }
    showModal(build_search_progress_modal())
    invisible(NULL)
  }

  output$search_progress_modal_body <- renderUI({
    if (!isTRUE(run_in_progress())) {
      return(NULL)
    }
    # Ensure ETA %/elapsed/ETA text refreshes even when stage changes are sparse.
    progress_tick()
    rl <- progress_run_lang()
    build_search_progress_body_ui(
      report_lang = rl,
      stage_id = progress_stage(),
      started_at = progress_started_at(),
      estimated_seconds = progress_estimated_sec(),
      model_label = openai_model_label(),
      cache_state = progress_cache_state(),
      repair_state = progress_repair_state(),
      failure_kind = progress_failure_kind()
    )
  })

  observeEvent(run_in_progress(), {
    if (isTRUE(run_in_progress())) {
      return(invisible(NULL))
    }
    removeModal()
    session$sendCustomMessage("closeProgressModal", list())
  }, ignoreInit = TRUE)

  schedule_waiting_model_stage <- function() {
    later::later(function() {
      if (!isTRUE(isolate(run_in_progress()))) {
        return(invisible(NULL))
      }
      if (identical(isolate(progress_stage()), "calling_openai")) {
        update_progress_state(stage = "waiting_model")
      }
    }, delay = 0.2)
  }

  update_progress_state <- function(stage = NULL,
                                    cache_state = NULL,
                                    repair_state = NULL,
                                    failure_kind = NULL) {
    if (!is.null(stage)) {
      progress_stage(stage)
    }
    if (!is.null(cache_state)) {
      progress_cache_state(cache_state)
    }
    if (!is.null(repair_state)) {
      progress_repair_state(repair_state)
    }
    if (!is.null(failure_kind)) {
      progress_failure_kind(failure_kind)
    }
    invisible(NULL)
  }

  finish_run <- function() {
    run_in_progress(FALSE)
    invisible(NULL)
  }

  observeEvent(input$run, {
    if (isTRUE(run_in_progress())) {
      return(invisible(NULL))
    }

    report_lang <- normalize_report_language(input$report_lang)
    model_label <- openai_model_label()
    run_in_progress(TRUE)
    progress_run_lang(report_lang)
    progress_started_at(Sys.time())
    news_count_input <- input$news_count
    progress_estimated_sec(estimate_run_duration_sec(news_count_input))
    progress_cache_state(NA_character_)
    progress_repair_state(NA_character_)
    progress_failure_kind(NA_character_)
    progress_stage("checking_inputs")
    show_progress_modal_now()

    country_iso3_raw <- input$country
    dr_input <- input$date_range
    all_topics_enabled <- isTRUE(input$all_topics)
    followup_enabled <- isTRUE(input$followup_enabled)
    topics_sel <- if (isTRUE(all_topics_enabled)) all_topic_ids else input$topics
    followup_topics_raw <- input$followup_topics

    if (!isTRUE(isolate(run_in_progress()))) {
      return(invisible(NULL))
    }

      country_iso3 <- country_iso3_raw
        if (is.null(country_iso3) || !nzchar(country_iso3)) {
          country_iso3 <- ""
        }
        start <- parse_ui_date(dr_input[1])
        end <- parse_ui_date(dr_input[2])
        start_iso <- as.character(start)
        end_iso <- as.character(end)
        start_ui <- format_ui_date(start)
        end_ui <- format_ui_date(end)
        follow <- process_followup_input(
          followup_enabled,
          followup_topics_raw
        )

    if (identical(country_iso3, "")) {
      status(tr(report_lang, "status_country_missing"))
      last_validated_response(NULL)
      update_progress_state(stage = "stopped")
      finish_run()
      return(invisible(NULL))
    }

    if (!country_iso3_valid(country_iso3)) {
      status(tr(report_lang, "status_country_invalid"))
      last_validated_response(NULL)
      update_progress_state(stage = "stopped")
      finish_run()
      return(invisible(NULL))
    }

    pv <- validate_period_span(start, end)
    if (!isTRUE(pv$ok)) {
      status(paste(pv$errors, collapse = "\n"))
      last_validated_response(NULL)
      update_progress_state(stage = "stopped")
      finish_run()
      return(invisible(NULL))
    }

    nc <- validate_news_count_input(news_count_input)
    if (!isTRUE(nc$ok)) {
      status(paste(nc$errors, collapse = "\n"))
      last_validated_response(NULL)
      update_progress_state(stage = "stopped")
      finish_run()
      return(invisible(NULL))
    }
    progress_estimated_sec(estimate_run_duration_sec(nc$n))

    tv <- validate_topic_selection(all_topics_enabled, topics_sel)
    if (!isTRUE(tv$ok)) {
      status(paste(tv$errors, collapse = "\n"))
      last_validated_response(NULL)
      update_progress_state(stage = "stopped")
      finish_run()
      return(invisible(NULL))
    }

    if (!follow$ok) {
      status(paste(c(tr(report_lang, "status_followup_prefix"), follow$errors), collapse = "\n"))
      last_validated_response(NULL)
      update_progress_state(stage = "stopped")
      finish_run()
      return(invisible(NULL))
    }

    fu_topics <- if (isTRUE(followup_enabled)) follow$topics else character()
    update_progress_state(stage = "loading_cache")
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
      update_progress_state(stage = "done", cache_state = "hit")
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
        model_label,
        topics_line,
        fu_line,
        format_run_elapsed(progress_started_at()),
        format_progress_tech_summary(
          report_lang,
          cache_state = progress_cache_state(),
          repair_state = progress_repair_state(),
          failure_kind = progress_failure_kind()
        )
      ))
      finish_run()
      return(invisible(NULL))
    }

    if (!openai_has_api_key()) {
      status(tr(report_lang, "status_api_key_missing"))
      last_validated_response(NULL)
      update_progress_state(stage = "stopped", cache_state = "miss")
      finish_run()
      return(invisible(NULL))
    }

    update_progress_state(stage = "calling_openai", cache_state = "miss")
    schedule_waiting_model_stage()

    prom <- promises::future_promise({
      fetch_sovereign_news_response(
        country_iso3 = country_iso3,
        date_start = start_iso,
        date_end = end_iso,
        all_topics = all_topics_enabled,
        topic_ids = topics_sel,
        n_news = nc$n,
        follow_up_enabled = followup_enabled,
        follow_up_queries = fu_topics,
        report_language = report_lang,
        progress_callback = NULL
      )
    })

    prom <- promises::then(prom, onFulfilled = function(api) {
      if (!isTRUE(isolate(run_in_progress()))) {
        return(invisible(NULL))
      }
      if (!isTRUE(api$ok)) {
          failure_kind <- as.character(api$telemetry$error_kind)[1L]
          if (is.na(failure_kind) || !nzchar(failure_kind)) {
            failure_kind <- "generic"
          }
          status(build_failure_status_message(
            report_lang = report_lang,
            failure_kind = failure_kind,
            error_message = api$error,
            elapsed_text = format_run_elapsed(progress_started_at()),
            cache_state = "miss",
            repair_state = if (isTRUE(api$telemetry$repair_attempted)) "attempted" else progress_repair_state()
          ))
          last_validated_response(NULL)
          update_progress_state(
            stage = "stopped",
            cache_state = "miss",
            repair_state = if (isTRUE(api$telemetry$repair_attempted)) "attempted" else progress_repair_state(),
            failure_kind = failure_kind
          )
        return(invisible(NULL))
      }

      update_progress_state(
        stage = "validating_response",
        cache_state = "miss",
        repair_state = if (isTRUE(api$telemetry$repair_attempted)) "attempted" else isolate(progress_repair_state())
      )

      parsed <- api$parsed
        chk <- validate_news_response(
          parsed,
          n_expected = nc$n,
          report_language = report_lang
        )
      if (!isTRUE(chk$ok)) {
        failure_kind <- "validation"
        status(paste(
          build_failure_status_message(
            report_lang = report_lang,
            failure_kind = failure_kind,
            error_message = tr(report_lang, "status_validation_failed"),
            elapsed_text = format_run_elapsed(isolate(progress_started_at())),
            cache_state = "miss",
            repair_state = if (isTRUE(api$telemetry$repair_attempted)) "attempted" else isolate(progress_repair_state())
          ),
          paste(chk$errors, collapse = "\n"),
          sep = "\n"
        ))
        last_validated_response(NULL)
        update_progress_state(
          stage = "stopped",
          cache_state = "miss",
          repair_state = if (isTRUE(api$telemetry$repair_attempted)) "attempted" else isolate(progress_repair_state()),
          failure_kind = failure_kind
        )
        return(invisible(NULL))
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
        model_label,
        topics_line,
        fu_line,
        format_run_elapsed(isolate(progress_started_at())),
        format_progress_tech_summary(
          report_lang,
          cache_state = isolate(progress_cache_state()),
          repair_state = isolate(progress_repair_state()),
          failure_kind = isolate(progress_failure_kind())
        ),
        uline
      ))

      update_progress_state(stage = "done")
      invisible(NULL)
    })

    prom <- promises::catch(prom, function(err) {
      if (!isTRUE(isolate(run_in_progress()))) {
        return(invisible(NULL))
      }
      err_msg <- tryCatch(conditionMessage(err), error = function(e) as.character(err)[1L])
      status(build_failure_status_message(
        report_lang = report_lang,
        failure_kind = "generic",
        error_message = err_msg,
        elapsed_text = format_run_elapsed(isolate(progress_started_at())),
        cache_state = "miss",
        repair_state = isolate(progress_repair_state())
      ))
      last_validated_response(NULL)
      update_progress_state(stage = "stopped", cache_state = "miss", failure_kind = "generic")
      invisible(NULL)
    })

    prom <- promises::finally(prom, onFinally = function() {
      finish_run()
    })

    invisible(NULL)
  })

  output$status <- renderText({
    if (isTRUE(run_in_progress())) {
      return("")
    }
    status()
  })

  observeEvent(current_report_lang(), {
    if (is.null(last_validated_response()) && !isTRUE(run_in_progress())) {
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
    rl <- if (isTRUE(run_in_progress())) progress_run_lang() else current_report_lang()
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
