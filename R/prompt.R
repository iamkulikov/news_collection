# Prompt assembly + strict JSON Schema for OpenAI structured output.
# Depends on: R/topics.R (`all_topic_ids`, `topic_choices`)

# --- topic metadata (ids must match `names(topic_choices)` / `all_topic_ids`) ----

topic_category_values <- c(
  "macro",
  "budget",
  "public_sector",
  "external",
  "institutions",
  "research_ifis",
  "follow_up_custom"
)

.topic_id_order <- c(
  macro = 1L,
  budget = 2L,
  public_sector = 3L,
  external = 4L,
  institutions = 5L,
  research_ifis = 6L
)

.topic_short_label_ru <- c(
  macro = "Макроэкономика",
  budget = "Бюджетная политика",
  public_sector = "Госсектор и неявные обязательства государства",
  external = "Внешняя позиция",
  institutions = "Институты, человеческий капитал и внешнеполитический контекст",
  research_ifis = "Исследования, Article IV МВФ и обзоры МФО по стране"
)


# --- embedded base prompt (source of truth; wording aligned with product spec) ----

.base_prompt_core_ru <- function() {
  paste0(
    "Ты аналитик с большим опытом анализа кредитоспособности государств. ",
    "Твоя роль — собрать наиболее важные новости из глобальных и локальных источников ",
    "и представить их в структурированном виде за заданный пользователем период ",
    "(по умолчанию ориентир — около полугода до сегодняшней даты, но даты всегда берутся из запроса). ",
    "Новости ищем по следующим темам:\n\n",
    "1) Макроэкономика. Новые прогнозы (если они существенно отличаются от старых). ",
    "Новые данные, если они содержали значимый сюрприз. Экономический рост, инфляция, валютный курс. ",
    "Изменения в экономической политике — ДКП или что-то общеэкономическое.\n\n",
    "2) Бюджетная политика. Изменения налоговых ставок. Новые планы по выпуску или погашению государственного долга. ",
    "Все, что может значимо повлиять на бюджетный баланс или динамику госдолга. ",
    "Крупные новые выпуски облигаций. Крупные новые кредиты.\n\n",
    "3) Госсектор. Новости госкомпаний или устойчивость стратегически важных компаний и секторов. ",
    "Все, что можно считать неявными условными обязательствами государства.\n\n",
    "4) Внешняя позиция. Валютный курс и валютная позиция. Барьеры во внешней торговле. ",
    "Пошлины и налогообложение внешней торговли. Планы по развитию торговли или ее ограничению. ",
    "Значимые изменения в динамике или структуре международных резервов. ",
    "Значимые входящие или исходящие внешние инвестиции.\n\n",
    "5) Институты. Все, что касается устойчивости государственной власти или напряженность в отношениях с другими государствами. ",
    "Все, что касается качества образования или долгосрочного качества человеческого капитала.\n\n",
    "6) Наиболее интересные новые научные или прикладные исследования по экономике страны. ",
    "Выход Article 4 IMF, выход специальных обзоров международных организаций, посвященных стране.\n\n",
    "По умолчанию нужно отобрать около 15 новостей (если пользователь не задал другое N). ",
    "Для каждой новости сделай численную оценку по шкале от 1 до 5 по следующим параметрам:\n\n",
    "1) Потенциальная важность для кредитоспособности\n",
    "2) Обсуждаемость или заметность\n",
    "3) Свежесть\n\n",
    "Новости нужно отранжировать по совокупной оценке трех параметров — от самых важных, свежих и обсуждаемых ",
    "к старым, менее свежим и незаметным.\n\n",
    "Если пользователь попросит отследить какие-то старые истории, то их нужно оценить по этим же параметрам ",
    "и добавить новости по ним в этот же список.\n\n",
    "Поиск нужно делать на английском языке и языке страны, по которой собираем новости ",
    "(конкретизация языков дается в пользовательском запросе).\n\n",
    "Ответ нужно давать на русском языке; сложные термины можно не переводить, либо в скобках давать исходные слова."
  )
}


.json_schema_instruction_ru <- function() {
  paste0(
    "Помимо связного текста на русском ты обязан вернуть валидируемый JSON строго по схеме, ",
    "которую мы задаем через structured output / JSON Schema на стороне клиента. ",
    "Поле items должно содержать ровно N элементов (N задается пользователем). ",
    "Каждый элемент должен включать оценки scores с целыми числами 1–5: ",
    "credit_importance (важность для кредитоспособности), visibility (обсуждаемость/заметность), freshness (свежесть). ",
    "Поле topic_category должно соответствовать одной из категорий таксономии (либо follow_up_custom, если событие пришло ",
    "в основном из ручного follow-up и не укладывается в сетку). ",
    "Поле composite_rank — целое число от 1 до N: итоговый порядок после объединения base-поиска и follow-up; ",
    "1 соответствует самому верхнему элементу списка (самый высокий совокупный приоритет по трем шкалам), N — самому нижнему. ",
    "Ранги должны быть согласованы с сортировкой items (без пропусков и дубликатов). ",
    "Для каждой новости укажи origin: base_search или follow_up. ",
    "Источники sources: обычно 2–6 записей; каждая запись содержит url и source_name (и published_date по возможности). ",
    "Не выдумывай URL: используй реальные ссылки, которые ты нашел в ходе поиска с инструментами провайдера."
  )
}


#' Build system prompt for sovereign-risk news collection.
#'
#' @return Character scalar.
sovereign_news_system_prompt <- function() {
  paste0(
    .base_prompt_core_ru(),
    "\n\n",
    .json_schema_instruction_ru()
  )
}


#' Format selected topics for the user message (Russian).
#'
#' @param all_topics Logical: use full taxonomy.
#' @param topic_ids Character vector of topic ids (`all_topic_ids` subset).
#' @return Character scalar.
format_topics_selection_ru <- function(all_topics, topic_ids) {
  if (isTRUE(all_topics)) {
    return("Используй все шесть тематических блоков (1–6) из системной инструкции.")
  }
  topic_ids <- unique(as.character(topic_ids))
  topic_ids <- topic_ids[!is.na(topic_ids) & nzchar(topic_ids)]
  if (!length(topic_ids)) {
    return("Темы не выбраны: это ошибка ввода; не выполняй поиск до уточнения тем.")
  }
  bad <- setdiff(topic_ids, names(.topic_id_order))
  if (length(bad)) {
    return(sprintf("Обнаружены неизвестные идентификаторы тем: %s.", paste(bad, collapse = ", ")))
  }
  ord <- order(match(topic_ids, names(.topic_id_order)))
  topic_ids <- topic_ids[ord]
  lines <- vapply(topic_ids, function(id) {
    sprintf("%d. %s", .topic_id_order[[id]], .topic_short_label_ru[[id]])
  }, character(1))
  paste0(
    "Ищи новости только по следующим тематическим блокам (остальные блоки 1–6 не используй):\n",
    paste(lines, collapse = "\n")
  )
}


#' Format follow-up block for the user message (Russian).
#'
#' @param queries Character vector (may be empty).
#' @return Character scalar.
format_follow_up_block_ru <- function(queries) {
  queries <- unique(as.character(queries))
  queries <- queries[!is.na(queries) & nzchar(queries)]
  if (!length(queries)) {
    return("Follow-up темы не заданы: дополнительный ручной список не используй.")
  }
  lines <- paste0("- ", queries, collapse = "\n")
  paste0(
    "Пользователь просит отдельно проверить обновления по следующим ранее актуальным сюжетам. ",
    "Оцени их по тем же шкалам 1–5 и включи в общий список (без дублирования одного и того же события), ",
    "с origin=follow_up, если новость относится именно к этому треку:\n",
    lines
  )
}


#' Assemble user prompt with concrete run parameters.
#'
#' @param country_iso3 ISO3 code.
#' @param country_display_name Human-readable country name for the model.
#' @param date_start,date_end Date or character coercible to Date.
#' @param all_topics Logical.
#' @param topic_ids Selected topic ids when `all_topics` is FALSE.
#' @param n_news Integer, number of news items required.
#' @param follow_up_queries Follow-up strings (ignored if `follow_up_enabled` is FALSE).
#' @param follow_up_enabled Whether follow-up list is active.
#' @param search_language_phrase Russian phrase listing languages for search, e.g.
#'   `"английском и португальском"`.
#' @return Character scalar.
build_sovereign_news_user_prompt <- function(country_iso3,
                                            country_display_name,
                                            date_start,
                                            date_end,
                                            all_topics = TRUE,
                                            topic_ids = all_topic_ids,
                                            n_news = default_news_count(),
                                            follow_up_queries = character(),
                                            follow_up_enabled = TRUE,
                                            search_language_phrase = "английском и основном языке (языках) страны") {
  country_iso3 <- toupper(trimws(as.character(country_iso3)))
  country_display_name <- as.character(country_display_name)[1L]
  if (is.na(country_display_name) || !nzchar(country_display_name)) {
    country_display_name <- country_iso3
  }

  start_chr <- as.character(as.Date(date_start))
  end_chr <- as.character(as.Date(date_end))
  n_news <- as.integer(n_news)[1L]
  if (is.na(n_news) || n_news < 1L) {
    n_news <- default_news_count()
  }

  topics_block <- format_topics_selection_ru(all_topics, topic_ids)
  fu <- if (isTRUE(follow_up_enabled)) {
    format_follow_up_block_ru(follow_up_queries)
  } else {
    "Follow-up отключен пользователем: игнорируй ручной список тем и не задавай origin=follow_up без оснований."
  }

  paste0(
    "Параметры запроса:\n",
    "- Страна (ISO3): ", country_iso3, "\n",
    "- Отображаемое имя страны: ", country_display_name, "\n",
    "- Период (включительно): с ", start_chr, " по ", end_chr, "\n",
    "- Требуемое количество новостей N: ", n_news, "\n\n",
    topics_block, "\n\n",
    fu, "\n\n",
    "Языки поиска: выполняй поиск на ", search_language_phrase, ".\n\n",
    "Верни итог строго в JSON по заданной схеме; текстовые поля по сути должны быть на русском."
  )
}


#' Full prompt bundle (system + user).
#'
#' @inheritParams build_sovereign_news_user_prompt
#' @return Named list: `system`, `user`.
build_sovereign_news_prompt <- function(country_iso3,
                                       country_display_name,
                                       date_start,
                                       date_end,
                                       all_topics = TRUE,
                                       topic_ids = all_topic_ids,
                                       n_news = default_news_count(),
                                       follow_up_queries = character(),
                                       follow_up_enabled = TRUE,
                                       search_language_phrase = "английском и основном языке (языках) страны") {
  list(
    system = sovereign_news_system_prompt(),
    user = build_sovereign_news_user_prompt(
      country_iso3 = country_iso3,
      country_display_name = country_display_name,
      date_start = date_start,
      date_end = date_end,
      all_topics = all_topics,
      topic_ids = topic_ids,
      n_news = n_news,
      follow_up_queries = follow_up_queries,
      follow_up_enabled = follow_up_enabled,
      search_language_phrase = search_language_phrase
    )
  )
}


# --- JSON Schema (OpenAI structured outputs style) --------------------------------

.json_schema_source_item <- function() {
  list(
    type = "object",
    properties = list(
      url = list(type = "string"),
      source_name = list(type = "string"),
      published_date = list(
        type = "string",
        description = "ISO date (YYYY-MM-DD) if known; otherwise empty string"
      )
    ),
    required = list("url", "source_name"),
    additionalProperties = FALSE
  )
}


.json_schema_scores <- function() {
  list(
    type = "object",
    properties = list(
      credit_importance = list(
        type = "integer",
        minimum = 1L,
        maximum = 5L,
        description = "Потенциальная важность для кредитоспособности (1–5)"
      ),
      visibility = list(
        type = "integer",
        minimum = 1L,
        maximum = 5L,
        description = "Обсуждаемость или заметность (1–5)"
      ),
      freshness = list(
        type = "integer",
        minimum = 1L,
        maximum = 5L,
        description = "Свежесть (1–5)"
      )
    ),
    required = list("credit_importance", "visibility", "freshness"),
    additionalProperties = FALSE
  )
}


.json_schema_item <- function(composite_rank_max = NULL) {
  composite_rank_prop <- list(
    type = "integer",
    minimum = 1L,
    description = "Итоговый ранг в общем списке после сортировки (1 — верх списка; совпадает с порядком items)"
  )
  if (!is.null(composite_rank_max) && !is.na(composite_rank_max) && composite_rank_max >= 1L) {
    composite_rank_prop$maximum <- as.integer(composite_rank_max)
  }

  list(
    type = "object",
    properties = list(
      title = list(type = "string"),
      date = list(
        type = "string",
        description = "ISO date (YYYY-MM-DD) if possible; otherwise best-effort or empty string"
      ),
      what_happened = list(type = "string"),
      why_it_matters_for_sovereign_risk = list(type = "string"),
      topic_category = list(
        type = "string",
        enum = as.list(topic_category_values)
      ),
      scores = .json_schema_scores(),
      composite_rank = composite_rank_prop,
      sources = list(
        type = "array",
        minItems = 2L,
        maxItems = 6L,
        items = .json_schema_source_item()
      ),
      origin = list(
        type = "string",
        enum = list("base_search", "follow_up")
      ),
      tags = list(
        type = "array",
        items = list(type = "string")
      ),
      notes_on_evidence_quality = list(type = "string")
    ),
    required = list(
      "title",
      "date",
      "what_happened",
      "why_it_matters_for_sovereign_risk",
      "topic_category",
      "scores",
      "composite_rank",
      "sources",
      "origin",
      "tags",
      "notes_on_evidence_quality"
    ),
    additionalProperties = FALSE
  )
}


#' JSON Schema object for the full model response (`response_format` / structured outputs).
#'
#' The schema requires exactly `n` items via `minItems`/`maxItems` when `n` is provided.
#'
#' @param n Optional integer: fixes `items` array length in the schema (min/max).
#' @return `list` suitable for `jsonlite::toJSON(..., auto_unbox = TRUE)`.
news_response_json_schema <- function(n = NULL) {
  n <- if (is.null(n)) NULL else as.integer(n)[1L]
  n_ok <- !is.null(n) && !is.na(n) && n >= 1L
  items_element_schema <- .json_schema_item(composite_rank_max = if (n_ok) n else NULL)

  items_array_schema <- list(
    type = "array",
    items = items_element_schema
  )
  if (n_ok) {
    items_array_schema$minItems <- n
    items_array_schema$maxItems <- n
  }

  schema <- list(
    type = "object",
    properties = list(
      country = list(
        type = "object",
        properties = list(
          iso3 = list(type = "string", minLength = 3L, maxLength = 3L),
          display_name = list(type = "string")
        ),
        required = list("iso3", "display_name"),
        additionalProperties = FALSE
      ),
      time_window = list(
        type = "object",
        properties = list(
          start = list(type = "string", description = "ISO date YYYY-MM-DD"),
          end = list(type = "string", description = "ISO date YYYY-MM-DD")
        ),
        required = list("start", "end"),
        additionalProperties = FALSE
      ),
      topics = list(
        type = "array",
        items = list(
          type = "string",
          enum = as.list(setdiff(topic_category_values, "follow_up_custom"))
        )
      ),
      follow_up_queries = list(
        type = "array",
        items = list(type = "string")
      ),
      items = items_array_schema
    ),
    required = list("country", "time_window", "topics", "follow_up_queries", "items"),
    additionalProperties = FALSE
  )

  schema
}


#' Wrapper for OpenAI `response_format`-style JSON schema objects.
#'
#' @param name Schema name for the API (`name` field).
#' @param n Optional item count constraint for `items`.
#' @return Named list: `name`, `schema`, optionally `strict`.
openai_news_response_format <- function(name = "sovereign_news_response", n = NULL) {
  list(
    type = "json_schema",
    json_schema = list(
      name = name,
      schema = news_response_json_schema(n = n),
      strict = TRUE
    )
  )
}


#' Responses API `text` block with sovereign news JSON schema.
#'
#' Depends on [openai_responses_text_json_schema()] from `R/openai_client.R` (load before this file).
#'
#' @inheritParams news_response_json_schema
#' @param name,strict Passed through to the client helper.
#' @return List for the `text` argument of [openai_responses_create()].
openai_responses_news_text <- function(n = NULL,
                                       name = "sovereign_news_response",
                                       strict = TRUE) {
  openai_responses_text_json_schema(name, news_response_json_schema(n = n), strict = strict)
}
