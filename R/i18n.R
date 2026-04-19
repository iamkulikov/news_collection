# Centralized UI/report i18n dictionary for RUS/ENG.

normalize_report_language <- function(report_lang) {
  rl <- toupper(trimws(as.character(report_lang)[1L]))
  if (!rl %in% c("RUS", "ENG")) {
    return("RUS")
  }
  rl
}


ui_i18n_dict <- list(
  RUS = c(
    app_title = "Country Risk News Finder",
    status_header = "Статус",
    results_header = "Результаты",
    export_header = "Экспорт",
    export_help = "Скачать последний валидированный ответ (те же данные, что в карточках).",
    export_json = "JSON",
    export_csv = "CSV",
    export_docx = "Word (DOCX)",
    status_initial = "Задайте страну, период и параметры слева, затем нажмите «Найти новости». После успешного запуска результат появится здесь.",
    results_progress_title = "Идет поиск новостей",
    results_progress_hint = "Запрос к OpenAI выполняется в фоне. Это может занять 1-3 минуты.",
    no_run = "Запуск еще не выполнялся.",
    no_items = "В этом ответе нет новостей.",
    follow_up_custom = "Дополнительная тема (пользовательская)",
    base_search = "Базовый поиск",
    follow_up = "Дополнительная тема",
    score_line = "Важность для кредитоспособности: %s · Заметность: %s · Свежесть: %s",
    what_happened = "Что произошло",
    why_matters = "Почему это важно для суверенного риска",
    sources = "Источники",
    no_links = "Ссылки отсутствуют.",
    tags = "Теги: ",
    evidence_quality = "Качество источников: ",
    docx_title = "Country Risk News Finder",
    docx_period = "%s (%s) — период: %s → %s",
    date = "Дата:",
    topic = "Тема:",
    origin = "Источник происхождения:",
    rank = "Ранг #%s",
    scores = "Оценки — важность для кредитоспособности: %s, заметность: %s, свежесть: %s",
    status_country_missing = "Выберите страну в боковой панели (поиск по названию или ISO2/ISO3 коду).",
    status_country_invalid = "Значение страны не распознано. Выберите запись из выпадающего списка, чтобы ISO-код был валидным.",
    status_followup_prefix = "Дополнительные темы:",
    status_cache_loaded = paste0(
      "Результат загружен из кэша (та же комбинация параметров в течение последних %s ч). ",
      "Префикс ключа кэша: %s…\n",
      "Сохранено: %s\n",
      "Использование API (из кэша): %s\n",
      "Сводка запуска — страна: %s | период: %s → %s | N: %s | язык отчета: %s | темы: %s%s\n",
      "Кэшированный ответ — параметры совпадают с предыдущим запуском в пределах TTL."
    ),
    status_api_key_missing = paste0(
      "OPENAI_API_KEY не задан. Добавьте API-ключ в `.Renviron` или в переменные окружения, ",
      "перезапустите R / Shiny-приложение и повторите запуск."
    ),
    status_api_failed = paste0(
      "Ошибка запроса к OpenAI:\n%s\n\n",
      "Подсказки: HTTP 400 — проверьте параметры/код ошибки; попробуйте OPENAI_MODEL, OPENAI_WEB_SEARCH=false, ",
      "OPENAI_JSON_SCHEMA_STRICT=false или OPENAI_WEB_SEARCH_TOOL_TYPE=web_search. ",
      "HTTP 403 — инструменты или модель недоступны. HTTP 429 — подождите или уменьшите N/период."
    ),
    status_validation_failed = "API вернул JSON, который не прошел валидацию (неожиданно):",
    status_finished = paste0(
      "Завершено в %s. Результат кэширован на %s ч.\n",
      "Сводка запуска — страна: %s | период: %s → %s | N: %s | элементов: %s | язык отчета: %s | темы: %s%s\n",
      "Использование API: %s"
    )
  ),
  ENG = c(
    app_title = "Country Risk News Finder",
    status_header = "Status",
    results_header = "Results",
    export_header = "Export",
    export_help = "Download the last validated response (same data as the cards).",
    export_json = "JSON",
    export_csv = "CSV",
    export_docx = "Word (DOCX)",
    status_initial = "Set the country, period, and options on the left, then click \"Find news\". Results appear here after a successful run.",
    results_progress_title = "Searching for news",
    results_progress_hint = "OpenAI request is running in the background. This may take 1-3 minutes.",
    no_run = "No run yet.",
    no_items = "No news items in this response.",
    follow_up_custom = "Follow-up (custom)",
    base_search = "Base search",
    follow_up = "Follow-up",
    score_line = "Credit importance: %s · Visibility: %s · Freshness: %s",
    what_happened = "What happened",
    why_matters = "Why it matters for sovereign risk",
    sources = "Sources",
    no_links = "No links.",
    tags = "Tags: ",
    evidence_quality = "Evidence quality: ",
    docx_title = "Country Risk News Finder",
    docx_period = "%s (%s) — period: %s → %s",
    date = "Date:",
    topic = "Topic:",
    origin = "Origin:",
    rank = "Rank #%s",
    scores = "Scores — credit importance: %s, visibility: %s, freshness: %s",
    status_country_missing = "Choose a country in the sidebar (search by name or ISO2/ISO3 code).",
    status_country_invalid = "That country value is not recognized. Pick an entry from the dropdown so the ISO code is valid.",
    status_followup_prefix = "Follow-up topics:",
    status_cache_loaded = paste0(
      "Loaded from cache (same request as within the last %s h). ",
      "Cache key starts with: %s…\n",
      "Saved at: %s\n",
      "API usage (stored): %s\n",
      "Run summary — country: %s | period: %s → %s | N: %s | report language: %s | topics: %s%s\n",
      "Cached response — same parameters as a previous run within the TTL."
    ),
    status_api_key_missing = paste0(
      "OPENAI_API_KEY is not set. Add your API key to `.Renviron` or the environment, ",
      "restart R / the Shiny app, then try again."
    ),
    status_api_failed = paste0(
      "OpenAI request failed:\n%s\n\n",
      "Hints: HTTP 400 — see param/code in the message; try OPENAI_MODEL, OPENAI_WEB_SEARCH=false, ",
      "OPENAI_JSON_SCHEMA_STRICT=false, or OPENAI_WEB_SEARCH_TOOL_TYPE=web_search. ",
      "HTTP 403 — tools or model not allowed. HTTP 429 — wait or reduce N / period."
    ),
    status_validation_failed = "The API returned JSON that failed validation (unexpected):",
    status_finished = paste0(
      "Finished at %s. Result cached for %s h.\n",
      "Run summary — country: %s | period: %s → %s | N: %s | items returned: %s | report language: %s | topics: %s%s\n",
      "API usage: %s"
    )
  )
)


tr <- function(report_lang, key, ...) {
  rl <- normalize_report_language(report_lang)
  val <- ui_i18n_dict[[rl]][[key]]
  if (is.null(val)) {
    val <- ui_i18n_dict[["ENG"]][[key]]
  }
  if (is.null(val)) {
    val <- key
  }
  args <- list(...)
  if (length(args)) {
    return(do.call(sprintf, c(val, args)))
  }
  val
}


topic_display_name <- function(topic_id, report_lang = "RUS") {
  id <- as.character(topic_id)[1L]
  id <- if (is.na(id)) "" else id
  rl <- normalize_report_language(report_lang)
  if (identical(id, "follow_up_custom")) {
    return(tr(rl, "follow_up_custom"))
  }

  if (identical(rl, "RUS")) {
    rus <- c(
      macro = "Макроэкономика",
      budget = "Бюджетная политика",
      public_sector = "Госсектор / неявные обязательства",
      external = "Внешняя позиция",
      institutions = "Институты / человеческий капитал / внешняя политика",
      research_ifis = "Исследования / Article IV IMF / обзоры МФО по стране"
    )
    if (!is.null(rus[[id]])) {
      return(rus[[id]])
    }
  }

  w <- which(topic_choices == id)
  if (length(w) == 1L) {
    return(names(topic_choices)[w])
  }
  id
}


origin_display_name <- function(origin, report_lang = "RUS") {
  id <- as.character(origin)[1L]
  id <- if (is.na(id)) "" else id
  switch(
    id,
    base_search = tr(report_lang, "base_search"),
    follow_up = tr(report_lang, "follow_up"),
    id
  )
}
