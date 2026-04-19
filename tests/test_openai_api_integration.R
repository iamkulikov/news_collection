# Optional live call to OpenAI Responses API (uses OPENAI_API_KEY, network billable).
#
# Default: skipped (exit 0) so CI and local runs without a key stay green.
#
# To run:
#   set OPENAI_API_INTEGRATION_TEST=1   (or "true" / "yes")
#   set OPENAI_API_KEY=sk-...
# Optional:
#   OPENAI_SMOKE_MODEL=gpt-4.1-mini   (cheaper model for smoke; else OPENAI_MODEL / default)
#   OPENAI_INTEGRATION_FAIL_ON_429=1  (by default HTTP 429 counts as “reached API” and exits 0)
#
# From repo root:
#   Rscript tests/test_openai_api_integration.R
#
# Windows PowerShell:
#   $env:OPENAI_API_INTEGRATION_TEST="1"; $env:OPENAI_API_KEY="..."; Rscript tests/test_openai_api_integration.R

wd <- getwd()
if (basename(wd) == "tests") {
  setwd(dirname(wd))
}

flag <- tolower(trimws(Sys.getenv("OPENAI_API_INTEGRATION_TEST", "")))
if (!flag %in% c("1", "true", "yes")) {
  message("Skipping OpenAI API integration test (set OPENAI_API_INTEGRATION_TEST=1 to enable).")
  quit(status = 0L)
}

suppressPackageStartupMessages({
  source("R/config.R", local = FALSE)
  source("R/openai_client.R", local = FALSE)
})

key <- openai_api_key()
if (is.na(key) || !nzchar(key)) {
  stop(
    "OPENAI_API_INTEGRATION_TEST is enabled but OPENAI_API_KEY is empty. ",
    "Set the key in the environment or .Renviron.",
    call. = FALSE
  )
}

smoke_model <- Sys.getenv("OPENAI_SMOKE_MODEL", "")
model_use <- if (nzchar(smoke_model)) {
  smoke_model
} else {
  openai_default_model()
}

# Minimal request: no tools (no web search), short reply — proves the API accepts our POST and returns 200 + body.
res <- openai_responses_create(
  instructions = "You are an API connectivity test. Obey the user message literally.",
  input = "Reply with exactly the single word PONG and nothing else.",
  model = model_use,
  tools = NULL,
  parse_json = FALSE,
  timeout_sec = min(90, openai_http_timeout_sec()),
  max_retries = 2L
)

if (!isTRUE(res$ok)) {
  st <- res$status
  err <- res$error %||% sprintf("HTTP %s", res$status %||% "?")
  fail_429 <- tolower(Sys.getenv("OPENAI_INTEGRATION_FAIL_ON_429", "")) %in% c("1", "true", "yes")
  # 429 can appear as HTTP status or inside transport/curl error text — still proves the request hit OpenAI.
  is_429 <- (!is.na(st) && st == 429L) || grepl("429|Too Many Requests", err, ignore.case = TRUE)
  if (!isTRUE(fail_429) && is_429) {
    message(
      "OpenAI returned HTTP 429 (rate limit). ",
      "The request reached api.openai.com; rerun later or set OPENAI_INTEGRATION_FAIL_ON_429=1 to fail on 429."
    )
    quit(status = 0L)
  }
  stop("OpenAI Responses API did not succeed: ", err, call. = FALSE)
}

if (is.null(res$status) || res$status != 200L) {
  stop("Expected HTTP 200, got ", res$status %||% "NULL", call. = FALSE)
}

out <- res$output_text
if (is.null(out) || length(out) != 1L || is.na(out[1L]) || !nzchar(out[1L])) {
  stop("API returned success but output_text is empty — cannot verify model reply.", call. = FALSE)
}

# Loose check: model followed instructions (word PONG somewhere in reply)
if (!grepl("\\bPONG\\b", out[1L], ignore.case = TRUE)) {
  stop(
    "Unexpected model output (expected to contain PONG): ",
    substr(out[1L], 1L, min(200L, nchar(out[1L]))),
    call. = FALSE
  )
}

usage_line <- format_openai_usage_text(res$usage)
if (nzchar(usage_line)) {
  message("API usage: ", usage_line)
} else {
  message("API call OK (usage fields not present in response body).")
}

message("OpenAI API integration test passed (model=", model_use, ").")
