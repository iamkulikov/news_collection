# Country Risk News Finder

**Structured country-news briefs for sovereign risk work — built in R, not assembled by hand.**

Personal research app: one run turns a country, period, and topic set into a ranked, source-linked brief (RUS/ENG), with JSON/CSV/DOCX export. The focus is the **analytical workflow** (coverage, ranking, evidence, repeatability); the stack exists to make that workflow fast and auditable.

![App screenshot](archive/assets/screen1.jpg)

## Why it exists

Country and sovereign risk analysis needs consistent news intake: comparable items, explicit sources, and ranking that reflects **credit relevance**, not just headlines. This tool automates collection and structuring so time goes to judgment, not copy-paste and reformatting.

## What you get

-   **Country-aware search** — names, ISO2/ISO3\
-   **Controlled window & topics** — macro, external, institutions, follow-up lines, etc.\
-   **Ranked output** — importance, visibility, freshness; optional timeline per story\
-   **Validated structured JSON** — schema checks and repair before anything hits the UI\
-   **Cache** — identical requests within TTL avoid duplicate API cost\
-   **Exports** — JSON, CSV, Word for notes and downstream work

## How it’s built (automation, not a black box)

| Layer | Choice |
|----|----|
| App | R · Shiny · bslib |
| Model | OpenAI Responses API + web search (`httr2`) |
| Quality | JSON schema validation, retry/repair, runtime tests in `archive/` |
| Repro | `renv.lock` |
| Host | [Posit Connect Cloud](https://connect.posit.cloud) — publish from RStudio |

Secrets (`OPENAI_API_KEY`, optional tuning) live in **environment variables** — Connect **Vars** in production, gitignored `.Renviron` locally. Details: [`deploy/POSIT_CONNECT.md`](deploy/POSIT_CONNECT.md).

## Run locally

``` r
source("renv/activate.R"); renv::restore(prompt = FALSE)
```

``` powershell
$env:OPENAI_API_KEY="sk-..."
Rscript -e "shiny::runApp('.')"
```

Or open `news_collection.Rproj` → **Run App** on `app.R`.

## Repo layout

-   **Production:** `app.R`, `R/`, `DESCRIPTION`, `renv.lock`, `renv/`\
-   **Deploy:** `deploy/POSIT_CONNECT.md`\
-   **Dev / tests / notes:** `archive/`

## Disclaimer

Personal research tool, not investment advice. API usage is billable; runs often take 1–3+ minutes depending on scope.
