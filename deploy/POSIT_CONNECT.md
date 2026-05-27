# Deploying to Posit Connect Cloud (RStudio)

This app is published as a **Shiny application** from RStudio to **Posit Connect Cloud** (`connect.posit.cloud`). Entry point: [`app.R`](../app.R) at the project root.

Development-only files (tests, scripts, shinyapps notes) live under [`archive/`](../archive/) and are **not** required on the server.

## Prerequisites

1.  **R** 4.x and RStudio with publishing to Posit Connect enabled.

2.  **Project root** as working directory (folder containing `app.R` and `R/`).

3.  **renv** — restore packages before publishing:

    ``` r
    source("renv/activate.R")
    renv::restore(prompt = FALSE)
    ```

4.  **`rsconnect`** — listed in `DESCRIPTION` under `Suggests`; install if RStudio does not load it:

    ``` r
    install.packages("rsconnect")
    ```

## Secrets (recommended: Connect Variables, not files)

The app reads configuration via `Sys.getenv()` in [`R/config.R`](../R/config.R). **Do not** commit API keys or put them in `app.R` / `R/*.R`.

| Where | What to do |
|----|----|
| **Posit Connect Cloud (production)** | Set **encrypted environment variables** on the content (see below). |
| **Local development** | `OPENAI_API_KEY` in a **gitignored** [`.Renviron`](../.gitignore) or `$env:OPENAI_API_KEY="sk-..."` in PowerShell. Never commit `.Renviron`. |

### Required

| Variable         | Purpose                             |
|------------------|-------------------------------------|
| `OPENAI_API_KEY` | OpenAI API key for live news search |

### Optional tuning

| Variable                      | Purpose                              |
|-------------------------------|--------------------------------------|
| `OPENAI_MODEL`                | Model name (default in `R/config.R`) |
| `OPENAI_TIMEOUT_SEC`          | HTTP timeout                         |
| `OPENAI_MAX_RETRIES`          | Retry count                          |
| `OPENAI_MAX_OUTPUT_TOKENS`    | Output token cap                     |
| `OPENAI_WEB_SEARCH`           | `true` / `false`                     |
| `OPENAI_WEB_SEARCH_TOOL_TYPE` | Tool type string                     |
| `OPENAI_JSON_SCHEMA_STRICT`   | Strict JSON schema                   |
| `NEWS_MAX_PERIOD_MONTHS`      | Max date span                        |
| `NEWS_CACHE_TTL_SEC`          | Request cache TTL (seconds)          |
| `NEWS_CACHE_DIR`              | Optional cache directory             |

### Setting variables on Connect Cloud

**When publishing from RStudio**

1.  **Publish** → select the Shiny app (`app.R`).
2.  Open **Advanced** (or equivalent) → **Variables**.
3.  Add a **secret** variable: name `OPENAI_API_KEY`, value = your key.
4.  Add any optional variables from the table above.
5.  Complete publish.

**After publish**

1.  Open the app on [connect.posit.cloud](https://connect.posit.cloud).
2.  **Settings** → **Vars** / **Advanced** → environment variables.
3.  Add or update `OPENAI_API_KEY` and optional vars; **restart** the app so workers reload env.

Values are encrypted at rest and injected at process start. They are **not** part of the uploaded source bundle when set through Connect.

## Publish from RStudio

1.  Open `news_collection.Rproj` (or set working directory to the repo root).
2.  `renv::restore()` if the library is out of date.
3.  Open `app.R` or use the project **Publish** control.
4.  Choose **Posit Connect Cloud** (account already linked in RStudio).
5.  Confirm **application files** include at least:
    -   `app.R`
    -   `R/` (all modules)
    -   `DESCRIPTION`
    -   `renv.lock` and `renv/` (for reproducible installs on the server)
6.  Ensure **Advanced → Variables** has `OPENAI_API_KEY` (secret).
7.  Publish and open the content URL from the publish dialog.

The local `rsconnect/` folder (deployment cache) is **gitignored**; RStudio recreates it on each machine.

## What not to upload

[`.rsconnectignore`](../.rsconnectignore) excludes non-runtime paths from the bundle, including:

-   `archive/` (tests, docs, shinyapps guide, screenshot)
-   `.Renviron`, `.git/`, editor folders
-   `news_collection.Rproj` (IDE metadata)

Do **not** add patterns that exclude `R/` or `R/config.R`.

## Smoke checklist (after publish)

1.  App loads; sidebar and results/export panels render.
2.  With `OPENAI_API_KEY` set on Connect, **Find news** does not fail immediately with “key missing”.
3.  Short run: country selected, default period, **N = 1** or **10** → results cards appear.
4.  Exports: JSON, CSV, DOCX download and open.
5.  Switch report language **RUS** / **ENG** on a completed run.
6.  **Logs** on Connect: no repeating unhandled errors when idle.

## Troubleshooting

| Symptom | What to try |
|----|----|
| `could not find function "news_count_limits"` | Publish from project root; `app.R` must `source("R/config.R")` before `R/app_ui.R`. Run `Rscript archive/scripts/verify_shiny_entrypoint.R` locally. |
| Key missing on server | Confirm variable name is exactly `OPENAI_API_KEY` on content **Vars**; restart app. |
| HTTP 429 / timeout | Reduce **N** or date range; tune `OPENAI_TIMEOUT_SEC` / `OPENAI_MAX_RETRIES`. |
| Web search 403/400 | `OPENAI_WEB_SEARCH=false` or change `OPENAI_WEB_SEARCH_TOOL_TYPE`. |
| Package errors on server | `renv::restore()` locally, `renv::snapshot()`, republish with updated `renv.lock`. |

## Local verification (optional)

From repo root:

``` powershell
Rscript archive/scripts/verify_renv.R
Rscript archive/scripts/verify_shiny_entrypoint.R
Rscript archive/tests/test_ux_regression_runtime.R
```

## Related files

-   [`app.R`](../app.R) — Shiny entry and `source()` order
-   [`deploy/POSIT_CONNECT.md`](POSIT_CONNECT.md) — this guide
-   [`archive/deploy/SHINYAPPS.md`](../archive/deploy/SHINYAPPS.md) — legacy shinyapps.io notes only
