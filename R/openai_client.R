# OpenAI Responses API client (httr2): timeouts, retries, structured `text.format`.

OPENAI_RESPONSES_URL <- "https://api.openai.com/v1/responses"

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Build `text` block with JSON Schema structured output (Responses API).
#'
#' Maps to `text = list(format = list(type = "json_schema", ...))`.
#'
#' @param name Schema name (allowed chars per OpenAI docs; max 64).
#' @param schema JSON Schema as an R `list` (`jsonlite`-serializable).
#' @param strict Whether to request strict schema adherence.
#' @return A list for the `text` argument of [openai_responses_create()].
openai_responses_text_json_schema <- function(name, schema, strict = TRUE) {
  list(format = list(
    type = "json_schema",
    name = name,
    schema = schema,
    strict = isTRUE(strict)
  ))
}

#' Default hosted web search tool for Responses API.
#'
#' @param type Tool type string accepted by the API for your model tier.
#' @return A one-element list suitable for `tools` in [openai_responses_create()].
openai_responses_default_web_search_tools <- function(type = "web_search_preview") {
  list(list(type = type))
}

#' Extract assistant text from a parsed Responses API JSON body.
#'
#' @param body Parsed JSON (`list`) from [httr2::resp_body_json()].
#' @return Character scalar; `NA_character_` if nothing found.
openai_responses_extract_output_text <- function(body) {
  if (!is.list(body)) return(NA_character_)
  ot <- body$output_text
  if (is.character(ot) && length(ot) >= 1L && nzchar(ot[1L])) {
    return(paste(ot, collapse = ""))
  }
  out <- body$output
  if (is.null(out) || !length(out)) return(NA_character_)
  parts <- character()
  for (item in out) {
    if (!is.list(item)) next
    if (identical(item$type, "message") || identical(item$role, "assistant")) {
      cont <- item$content
      if (is.null(cont)) next
      if (is.character(cont)) {
        parts <- c(parts, cont)
      } else if (is.list(cont)) {
        for (block in cont) {
          if (!is.list(block)) next
          if (identical(block$type, "output_text") && length(block$text)) {
            parts <- c(parts, as.character(block$text))
          }
        }
      }
    }
  }
  if (!length(parts)) return(NA_character_)
  paste(parts, collapse = "")
}

#' Extract token / cost fields from a Responses API response body (if present).
#'
#' @param body Parsed JSON from [httr2::resp_body_json()].
#' @return Named `list` suitable for logging/UI, or `NULL` if nothing found.
openai_responses_extract_usage <- function(body) {
  if (!is.list(body)) {
    return(NULL)
  }
  u <- body$usage
  out <- list()
  if (is.list(u)) {
    for (nm in c("input_tokens", "output_tokens", "total_tokens")) {
      if (!is.null(u[[nm]])) {
        vv <- suppressWarnings(as.integer(round(as.numeric(u[[nm]]))))
        if (!is.na(vv)) out[[nm]] <- vv
      }
    }
    if (is.null(out$input_tokens) && !is.null(u$prompt_tokens)) {
      vv <- suppressWarnings(as.integer(round(as.numeric(u$prompt_tokens))))
      if (!is.na(vv)) out$input_tokens <- vv
    }
    if (is.null(out$output_tokens) && !is.null(u$completion_tokens)) {
      vv <- suppressWarnings(as.integer(round(as.numeric(u$completion_tokens))))
      if (!is.na(vv)) out$output_tokens <- vv
    }
    for (nm in c("cost", "total_cost")) {
      if (!is.null(u[[nm]]) && is.numeric(u[[nm]])) {
        out[[nm]] <- as.numeric(u[[nm]])[1L]
        break
      }
    }
  }
  if (!is.null(body$cost) && is.numeric(body$cost)) {
    out$cost_usd <- as.numeric(body$cost)[1L]
  }
  if (length(out)) out else NULL
}

#' Combine usage from two API calls (e.g. main + JSON repair) for display.
#'
#' Sums input/output tokens and sets `total_tokens` to their sum when both exist.
openai_usage_add <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)
  it <- sum(c(a$input_tokens, b$input_tokens), na.rm = TRUE)
  ot <- sum(c(a$output_tokens, b$output_tokens), na.rm = TRUE)
  out <- list()
  if (is.finite(it)) out$input_tokens <- as.integer(it)
  if (is.finite(ot)) out$output_tokens <- as.integer(ot)
  if (!is.null(out$input_tokens) && !is.null(out$output_tokens)) {
    out$total_tokens <- out$input_tokens + out$output_tokens
  }
  out
}

#' Human-readable usage line for status text (English, short).
format_openai_usage_text <- function(usage) {
  if (is.null(usage) || !length(usage)) {
    return("")
  }
  parts <- character()
  if (!is.null(usage$input_tokens) && !is.null(usage$output_tokens)) {
    parts <- c(
      parts,
      sprintf(
        "tokens in=%s out=%s",
        usage$input_tokens, usage$output_tokens
      )
    )
  } else if (!is.null(usage$total_tokens)) {
    parts <- c(parts, sprintf("tokens total=%s", usage$total_tokens))
  }
  cst <- usage$cost %||% usage$total_cost %||% usage$cost_usd
  if (!is.null(cst) && is.finite(as.numeric(cst))) {
    parts <- c(parts, sprintf("cost=%s", format(as.numeric(cst), scientific = FALSE, digits = 6)))
  }
  paste(parts, collapse = "; ")
}

#' Parse JSON from model output text.
#'
#' @param text Character scalar.
#' @return Parsed object (list).
openai_responses_parse_json_text <- function(text) {
  jsonlite::fromJSON(text, simplifyVector = FALSE, simplifyDataFrame = FALSE)
}

.openai_http_error_hint <- function(status) {
  if (status == 401L) {
    return(" Check OPENAI_API_KEY and account access.")
  }
  if (status == 403L) {
    return(" Your account may not have access to this model or tool (e.g. web search).")
  }
  if (status == 429L) {
    return(" Rate limited — wait a moment, reduce N or the date range, then retry.")
  }
  if (status %in% c(500L, 502L, 503L, 504L)) {
    return(" OpenAI had a temporary problem — retry shortly.")
  }
  ""
}

.openai_format_error_message <- function(body, status) {
  hint <- .openai_http_error_hint(status)
  if (is.null(body)) {
    return(sprintf("OpenAI API error (HTTP %s).%s", status, hint))
  }
  err <- body$error
  if (is.list(err) && length(err$message)) {
    return(sprintf("OpenAI API error (HTTP %s): %s%s", status, err$message, hint))
  }
  sprintf("OpenAI API error (HTTP %s).%s", status, hint)
}

.openai_format_transport_error <- function(msg) {
  if (is.null(msg) || !nzchar(msg)) {
    return("Request failed (no details). Check your network and try again.")
  }
  m <- as.character(msg)[1L]
  hint <- ""
  if (grepl("timed out|Timeout|timeout|Timeout was reached", m, ignore.case = TRUE)) {
    hint <- paste0(
      " Try a shorter period or fewer news items, increase OPENAI_TIMEOUT_SEC, ",
      "or retry when the service is less busy."
    )
  } else if (grepl(
    "Could not resolve|Connection refused|Failed to connect|Name or service not known",
    m,
    ignore.case = TRUE
  )) {
    hint <- " Check your internet connection, proxy/VPN, and firewall settings."
  } else if (grepl("SSL|certificate", m, ignore.case = TRUE)) {
    hint <- " There may be a TLS/proxy issue on this machine."
  }
  paste0("OpenAI request could not complete: ", m, hint)
}

#' Create a response via `POST /v1/responses`.
#'
#' Uses `OPENAI_API_KEY`, optional `OPENAI_MODEL`, `OPENAI_TIMEOUT_SEC`,
#' `OPENAI_MAX_RETRIES` via [openai_api_key()], [openai_default_model()],
#' [openai_http_timeout_sec()], [openai_max_retries()] when arguments are NULL.
#'
#' @param instructions Optional system/developer instructions (string).
#' @param input User input: string (user message) or `input` array per API.
#' @param model Model id.
#' @param text Optional `text` object (`openai_responses_text_json_schema()` for structured output).
#' @param tools Optional list of tools (e.g. [openai_responses_default_web_search_tools()]).
#' @param tool_choice Optional `tool_choice` value.
#' @param api_key Overrides env key.
#' @param timeout_sec, max_retries HTTP timeout and retry budget for transient errors.
#' @param parse_json If `TRUE`, parse `output_text` as JSON with [jsonlite::fromJSON()].
#' @param json_repair_attempt If `TRUE` and parsing fails or `validate_parsed_json` rejects the
#'   object, one follow-up call without tools asks the model to fix JSON (same schema if `text`
#'   was provided).
#' @param validate_parsed_json Optional `function(parsed)` returning `list(ok = logical, errors = character())`
#'   (see [validate_news_response()]). When `ok` is `FALSE`, the result is treated like a failed parse
#'   and triggers the same single repair attempt when `json_repair_attempt` is `TRUE`.
#' @param extra_body Additional named list merged into the JSON body (last wins on duplicate names).
#'
#' @return A list: `ok`, `status`, `body`, `output_text`, `parsed_json`, `parse_error`,
#'   `error` (character or `NULL`), `response` (httr2 response on success path, or last error path),
#'   `usage` (from [openai_responses_extract_usage()] when the API returns usage; combined
#'   across main + repair when both ran).
openai_responses_create <- function(instructions = NULL,
                                   input,
                                   model = NULL,
                                   text = NULL,
                                   tools = NULL,
                                   tool_choice = NULL,
                                   api_key = NULL,
                                   timeout_sec = NULL,
                                   max_retries = NULL,
                                   parse_json = FALSE,
                                   json_repair_attempt = FALSE,
                                   validate_parsed_json = NULL,
                                   extra_body = NULL) {
  if (missing(input) || is.null(input)) {
    stop("`input` is required.", call. = FALSE)
  }
  key <- if (is.null(api_key)) openai_api_key() else api_key
  if (is.na(key) || !nzchar(key)) {
    stop("OPENAI_API_KEY is not set.", call. = FALSE)
  }
  model_use <- if (is.null(model)) openai_default_model() else model
  timeout_sec <- if (is.null(timeout_sec)) openai_http_timeout_sec() else timeout_sec
  max_retries <- if (is.null(max_retries)) openai_max_retries() else max_retries

  body <- list(model = model_use, input = input)
  if (!is.null(instructions)) body$instructions <- instructions
  if (!is.null(text)) body$text <- text
  if (!is.null(tools)) body$tools <- tools
  if (!is.null(tool_choice)) body$tool_choice <- tool_choice
  if (is.list(extra_body) && length(extra_body)) {
    body <- utils::modifyList(body, extra_body)
  }

  req <- httr2::request(OPENAI_RESPONSES_URL)
  req <- httr2::req_headers(req, Authorization = paste("Bearer", key))
  req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
  req <- httr2::req_timeout(req, timeout_sec)
  req <- httr2::req_retry(
    req,
    max_tries = max_retries,
    is_transient = function(resp) {
      sc <- httr2::resp_status(resp)
      sc %in% c(429, 500, 502, 503, 504)
    }
  )

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      list(.err = conditionMessage(e))
    }
  )

  if (is.list(resp) && !is.null(resp$.err)) {
    return(list(
      ok = FALSE,
      status = NA_integer_,
      body = NULL,
      output_text = NA_character_,
      parsed_json = NULL,
      parse_error = NULL,
      error = .openai_format_transport_error(resp$.err),
      response = NULL,
      usage = NULL
    ))
  }

  status <- httr2::resp_status(resp)
  body_parsed <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (status >= 400L) {
    return(list(
      ok = FALSE,
      status = status,
      body = body_parsed,
      output_text = NA_character_,
      parsed_json = NULL,
      parse_error = NULL,
      error = .openai_format_error_message(body_parsed, status),
      response = resp,
      usage = openai_responses_extract_usage(body_parsed)
    ))
  }

  out_txt <- openai_responses_extract_output_text(body_parsed)
  if (isTRUE(parse_json) && (is.na(out_txt) || !nzchar(out_txt))) {
    return(list(
      ok = FALSE,
      status = status,
      body = body_parsed,
      output_text = out_txt,
      parsed_json = NULL,
      parse_error = NULL,
      error = "Empty model output; cannot parse JSON.",
      response = resp,
      repair_attempted = FALSE,
      usage = openai_responses_extract_usage(body_parsed)
    ))
  }

  pj <- NULL
  pj_err <- NULL
  if (isTRUE(parse_json) && !is.na(out_txt) && nzchar(out_txt)) {
    pj <- tryCatch(
      openai_responses_parse_json_text(out_txt),
      error = function(e) {
        pj_err <<- conditionMessage(e)
        NULL
      }
    )
  }

  if (isTRUE(parse_json) && !is.null(pj) && !is.null(validate_parsed_json)) {
    vr <- tryCatch(
      validate_parsed_json(pj),
      error = function(e) {
        list(ok = FALSE, errors = conditionMessage(e))
      }
    )
    if (!is.list(vr) || is.null(vr$ok)) {
      pj <- NULL
      pj_err <- "validate_parsed_json must return list(ok = logical, errors = character())."
    } else if (!isTRUE(vr$ok)) {
      pj <- NULL
      errs <- vr$errors
      if (is.null(errs) || !length(errs)) errs <- "unknown validation error"
      if (!is.character(errs)) errs <- as.character(errs)
      pj_err <- paste0("Validation failed: ", paste(errs, collapse = "; "))
    }
  }

  if (isTRUE(parse_json) && is.null(pj) && !is.na(out_txt) && nzchar(out_txt) &&
      !isTRUE(json_repair_attempt)) {
    return(list(
      ok = FALSE,
      status = status,
      body = body_parsed,
      output_text = out_txt,
      parsed_json = NULL,
      parse_error = pj_err,
      error = paste0("JSON processing failed: ", pj_err %||% "unknown"),
      response = resp,
      repair_attempted = FALSE,
      usage = openai_responses_extract_usage(body_parsed)
    ))
  }

  if (isTRUE(parse_json) && isTRUE(json_repair_attempt) && is.null(pj) &&
      !is.na(out_txt) && nzchar(out_txt)) {
    repair_instructions <- paste0(
      "Исправь выход так, чтобы он был одним валидным JSON-объектом без Markdown и без пояснений. ",
      "Сохрани смысл данных и полное соответствие схеме structured output (включая все обязательные поля и ограничения)."
    )
    if (!is.null(instructions)) {
      repair_instructions <- paste(instructions, repair_instructions, sep = "\n\n")
    }
    repair_input <- paste0(
      "Нужно исправить вывод модели. Ошибка: ", pj_err %||% "unknown", ".\n\n",
      "Исходный текст ответа:\n", out_txt
    )
    repair_body <- list(
      model = model_use,
      instructions = repair_instructions,
      input = repair_input,
      tool_choice = "none"
    )
    if (!is.null(text)) repair_body$text <- text
    if (is.list(extra_body) && length(extra_body)) {
      repair_body <- utils::modifyList(repair_body, extra_body)
    }

    req2 <- httr2::request(OPENAI_RESPONSES_URL)
    req2 <- httr2::req_headers(req2, Authorization = paste("Bearer", key))
    req2 <- httr2::req_body_json(req2, repair_body, auto_unbox = TRUE)
    req2 <- httr2::req_timeout(req2, timeout_sec)
    req2 <- httr2::req_retry(
      req2,
      max_tries = max_retries,
      is_transient = function(resp) {
        sc <- httr2::resp_status(resp)
        sc %in% c(429, 500, 502, 503, 504)
      }
    )

    resp2 <- tryCatch(
      httr2::req_perform(req2),
      error = function(e) {
        list(.err = conditionMessage(e))
      }
    )

    if (is.list(resp2) && !is.null(resp2$.err)) {
      return(list(
        ok = FALSE,
        status = NA_integer_,
        body = body_parsed,
        output_text = out_txt,
        parsed_json = NULL,
        parse_error = pj_err,
        error = paste0(
          "JSON processing failed; repair request failed: ",
          .openai_format_transport_error(resp2$.err)
        ),
        response = resp,
        usage = openai_responses_extract_usage(body_parsed)
      ))
    }

    st2 <- httr2::resp_status(resp2)
    body2 <- tryCatch(
      httr2::resp_body_json(resp2, simplifyVector = FALSE),
      error = function(e) NULL
    )

    if (st2 >= 400L) {
      u1 <- openai_responses_extract_usage(body_parsed)
      u2 <- openai_responses_extract_usage(body2)
      return(list(
        ok = FALSE,
        status = st2,
        body = body2,
        output_text = out_txt,
        parsed_json = NULL,
        parse_error = pj_err,
        error = paste0("JSON processing failed; repair HTTP error: ", .openai_format_error_message(body2, st2)),
        response = resp2,
        usage = openai_usage_add(u1, u2),
        usage_primary = u1,
        usage_repair = u2
      ))
    }

    out_txt2 <- openai_responses_extract_output_text(body2)
    pj2 <- NULL
    pj_err2 <- NULL
    if (!is.na(out_txt2) && nzchar(out_txt2)) {
      pj2 <- tryCatch(
        openai_responses_parse_json_text(out_txt2),
        error = function(e) {
          pj_err2 <<- conditionMessage(e)
          NULL
        }
      )
    }

    if (!is.null(pj2) && !is.null(validate_parsed_json)) {
      vr2 <- tryCatch(
        validate_parsed_json(pj2),
        error = function(e) {
          list(ok = FALSE, errors = conditionMessage(e))
        }
      )
      if (!is.list(vr2) || is.null(vr2$ok)) {
        pj2 <- NULL
        pj_err2 <- "validate_parsed_json must return list(ok = logical, errors = character())."
      } else if (!isTRUE(vr2$ok)) {
        pj2 <- NULL
        errs2 <- vr2$errors
        if (is.null(errs2) || !length(errs2)) errs2 <- "unknown validation error"
        if (!is.character(errs2)) errs2 <- as.character(errs2)
        pj_err2 <- paste0("Validation failed: ", paste(errs2, collapse = "; "))
      }
    }

    u1 <- openai_responses_extract_usage(body_parsed)
    u2 <- openai_responses_extract_usage(body2)
    return(list(
      ok = !is.null(pj2),
      status = st2,
      body = body2,
      output_text = out_txt2,
      parsed_json = pj2,
      parse_error = pj_err2,
      error = if (is.null(pj2)) {
        paste0("JSON processing failed after repair: ", pj_err2 %||% "unknown")
      } else {
        NULL
      },
      response = resp2,
      repair_attempted = TRUE,
      first_parse_error = pj_err,
      first_body = body_parsed,
      first_output_text = out_txt,
      usage = openai_usage_add(u1, u2),
      usage_primary = u1,
      usage_repair = u2
    ))
  }

  list(
    ok = TRUE,
    status = status,
    body = body_parsed,
    output_text = out_txt,
    parsed_json = pj,
    parse_error = pj_err,
    error = NULL,
    response = resp,
    repair_attempted = FALSE,
    usage = openai_responses_extract_usage(body_parsed)
  )
}
