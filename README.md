# Country Risk News Finder

[![R](https://img.shields.io/badge/R-4.x-blue)](https://www.r-project.org/) [![License](https://img.shields.io/badge/License-Not%20specified-lightgrey)](#notes)

Small personal R/Shiny app for collecting and ranking country-level economic news with OpenAI Responses API + web search.

The app helps build a structured news brief for sovereign risk analysis: - choose a country (name or ISO), - choose a time window, - choose all topics or a subset, - optionally add follow-up topics, - set the number of news items, - get a ranked report with source links and export it as JSON/CSV/DOCX.

## What it does

-   **Country-aware search**: supports ISO2/ISO3 and country names.
-   **Time window control**: default last 6 months, with max span limits.
-   **Topic filtering**: run on all topics or selected categories.
-   **Follow-up mode**: add manual "old stories" to track in the same run.
-   **Bilingual output**: `RUS`/`ENG` report language switch.
-   **Structured ranking**: each news item includes importance/visibility/freshness scores.
-   **Validation and repair flow**: validates model JSON output, with retry/repair logic.
-   **Request cache**: avoids re-calling API for identical requests within TTL.
-   **Exports**: JSON, CSV, and DOCX.

## Screenshots

Minimal placeholders (you can replace these files with real assets anytime):

![App screenshot](docs/screenshot.png)

![App demo](docs/demo.gif)

## Tech stack

-   **Language**: R
-   **UI**: Shiny + bslib
-   **LLM/API**: OpenAI Responses API (`httr2`)
-   **Data handling**: `jsonlite`, `digest`
-   **Country mapping**: `countrycode`
-   **DOCX export**: `officer`

Package dependencies are listed in `DESCRIPTION`.

## Project structure

-   `app.R` — app entry point
-   `R/app_ui.R` — UI layout and styling
-   `R/app_server.R` — server logic, progress states, caching flow
-   `R/prompt.R` — prompt assembly
-   `R/openai_client.R` — OpenAI Responses client
-   `R/schema.R` — response schema and validation
-   `R/render.R` — result rendering + CSV/DOCX output helpers
-   `R/config.R` — defaults, limits, env-backed settings
-   `R/cache.R` — request cache
-   `tests/` — runtime/integration test scripts

## Quick start

1.  Install R dependencies:

    ``` r
    install.packages(c("shiny", "bslib", "countrycode", "digest", "httr2", "jsonlite", "officer", "yaml"))
    ```

2.  Set your API key:

    -   Windows PowerShell:

        ``` powershell
        $env:OPENAI_API_KEY="sk-..."
        ```

    -   or add `OPENAI_API_KEY=...` to `.Renviron`.

3.  Run the app from project root:

    ``` powershell
    Rscript -e "shiny::runApp('.')"
    ```

## Configuration (env vars)

Common options:

-   `OPENAI_API_KEY` — required for live requests.
-   `OPENAI_MODEL` — model name (default from `R/config.R`).
-   `OPENAI_TIMEOUT_SEC` — request timeout.
-   `OPENAI_MAX_RETRIES` — retry attempts.
-   `OPENAI_WEB_SEARCH` — enable/disable web search tool.
-   `OPENAI_WEB_SEARCH_TOOL_TYPE` — tool type string.
-   `OPENAI_JSON_SCHEMA_STRICT` — strict schema mode.
-   `NEWS_MAX_PERIOD_MONTHS` — max allowed date span.
-   `NEWS_CACHE_TTL_SEC` — cache TTL in seconds.

See `R/config.R` for defaults and validation.

## Tests

Project includes runtime scripts in `tests/`, including: - API client/runtime checks, - cache and follow-up checks, - UX regression checks, - DOCX formatting checks, - optional live OpenAI integration test.

Run example:

``` powershell
Rscript tests/test_openai_client_runtime.R
```

For live API smoke test:

``` powershell
$env:OPENAI_API_INTEGRATION_TEST="1"
$env:OPENAI_API_KEY="sk-..."
Rscript tests/test_openai_api_integration.R
```

## Notes {#notes}

-   This is a personal research tool, not financial advice.
-   API calls may be billable and may take 1-3 minutes depending on request size.
-   License is not specified yet (update this when you add a `LICENSE` file).
