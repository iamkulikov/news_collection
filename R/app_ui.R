# UI: layout, bslib theme, baseline inputs/outputs

app_ui <- function(request) {
  
  limits <- news_count_limits()
  dr <- default_date_range()
  
  page_sidebar(
    title = tr("RUS", "app_title"),
    theme = bs_theme(
      version = 5,
      bootswatch = "zephyr",
      primary = "#0d6efd",
      "navbar-bg" = "#0b2239"
    ),
    sidebar = sidebar(
      width = 360,
      selectizeInput(
        inputId = "country",
        label = "Country",
        choices = country_choices,
        selected = default_country_choice(),
        options = list(
          placeholder = "Country name, ISO2 (BR), or ISO3 (BRA)",
          maxOptions = length(country_choices)
        )
      ),
      dateRangeInput(
        inputId = "date_range",
        label = paste0("Period (maximum span: ", max_period_months(), " months)"),
        start = dr[["start"]],
        end = dr[["end"]],
        max = Sys.Date(),
        format = ui_date_format(),
        separator = " - ",
        weekstart = 1
      ),
      actionButton("btn_last_6m", "Last 6 months", class = "btn-outline-secondary btn-sm w-100 mb-3"),
      checkboxInput("all_topics", "Search all topics", value = TRUE),
      conditionalPanel(
        condition = "input.all_topics == false",
        tagList(
          checkboxGroupInput(
            inputId = "topics",
            label = "Topics",
            choices = topic_choices,
            selected = all_topic_ids
          ),
          fluidRow(
            column(6, actionLink("topics_select_all", "Select all")),
            column(6, actionLink("topics_clear", "Clear all"))
          )
        )
      ),
      checkboxInput("followup_enabled", "Include follow-up topics", value = FALSE),
      conditionalPanel(
        condition = "input.followup_enabled == true",
        textAreaInput(
          inputId = "followup_topics",
          label = "Follow-up topics (one per line or separated by `;`)",
          rows = 4,
          placeholder = "e.g. banking stress; IMF talks; capital controls"
        )
      ),
      numericInput(
        inputId = "news_count",
        label = "Number of news items (N)",
        value = default_news_count(),
        min = limits$min,
        max = limits$max,
        step = 1L
      ),
      selectInput(
        inputId = "report_lang",
        label = "Output report language",
        choices = c("RUS", "ENG"),
        selected = "RUS"
      ),
      if (!openai_has_api_key()) {
        p(
          class = "text-warning small mb-2",
          "OPENAI_API_KEY is not set — “Find news” cannot load results until the key is configured."
        )
      },
      actionButton("run", "Find news", class = "btn-primary w-100")
    ),
    tags$head(
      tags$style(HTML(
        ".results-panel, .export-panel { min-height: 32rem; }\
         .export-panel {\
           position: sticky;\
           top: 1rem;\
           align-self: start;\
           max-height: calc(100vh - 2rem);\
         }\
         .status-panel-horizontal { min-height: 12rem; }\
         .status-scroll { max-height: 14rem; overflow-y: auto; }\
         .status-scroll pre { white-space: pre-wrap; word-break: break-word; }\
         .results-scroll { min-height: 26rem; max-height: 70vh; overflow-y: auto; }\
         .export-scroll { max-height: 70vh; overflow-y: auto; }\
         .export-actions-sticky {\
           position: sticky;\
           top: 0;\
           z-index: 5;\
           background: var(--bs-body-bg);\
           border-bottom: 1px solid var(--bs-border-color);\
           padding-bottom: .75rem;\
           margin-bottom: .75rem;\
         }\
         @media (max-width: 991.98px) {\
           .export-panel {\
             position: static;\
             max-height: none;\
           }\
           .export-actions-sticky {\
             position: sticky;\
             bottom: 0;\
             top: auto;\
             border-top: 1px solid var(--bs-border-color);\
             border-bottom: 0;\
             margin-top: .75rem;\
             margin-bottom: 0;\
             padding-top: .75rem;\
             padding-bottom: 0;\
           }\
         }\
         .selectize-control .selectize-dropdown .selectize-dropdown-content {\
           max-height: min(60vh, 24rem) !important;\
           overflow-y: auto !important;\
           overscroll-behavior: contain;\
         }\
         .modal-dialog.search-progress-modal .modal-body {\
           min-height: 10rem;\
         }\
         .search-progress-widget .progress {\
           height: 1.25rem;\
           background-color: #e8eef5;\
           border: 1px solid #cfd9e5;\
           border-radius: 999px;\
           overflow: hidden;\
         }\
         .search-progress-widget .progress-bar {\
           background-color: #0d6efd;\
           transition: width .6s linear;\
         }\
         .topic-badge {\
           border: 1px solid transparent;\
         }\
         .topic-badge--macro {\
           background-color: #e7f0ff;\
           border-color: #c3d6fb;\
           color: #1f4d8f;\
         }\
         .topic-badge--budget {\
           background-color: #fff3d8;\
           border-color: #f1d9a6;\
           color: #8a5a0a;\
         }\
         .topic-badge--public-sector {\
           background-color: #f2e9ff;\
           border-color: #d9c6ff;\
           color: #6c3aa6;\
         }\
         .topic-badge--external {\
           background-color: #e7f8ef;\
           border-color: #bfdccf;\
           color: #206b49;\
         }\
         .topic-badge--institutions {\
           background-color: #fdecef;\
           border-color: #f2c8d2;\
           color: #a33f62;\
         }\
         .topic-badge--research-ifis {\
           background-color: #edf1f6;\
           border-color: #d2dbe5;\
           color: #4a5a70;\
         }\
         .topic-badge--follow-up-custom {\
           background-color: #fff0e2;\
           border-color: #f0d0ad;\
           color: #975d15;\
         }\
         .topic-badge--default {\
           background-color: #eef1f4;\
           border-color: #d7dee6;\
           color: #4f5f73;\
         }"
      )),
    tags$script(HTML(
      "(function() {\
         if (typeof Shiny !== 'undefined') {\
           Shiny.addCustomMessageHandler('closeProgressModal', function() {\
             if (window.Shiny && Shiny.modal && typeof Shiny.modal.remove === 'function') {\
               Shiny.modal.remove();\
             }\
             document.querySelectorAll('.modal-backdrop').forEach(function(el) {\
               el.remove();\
             });\
             document.body.classList.remove('modal-open');\
             document.body.style.removeProperty('padding-right');\
           });\
         }\
       })();"
    ))
    ),
    layout_columns(
      col_widths = c(9, 3),
      gap = "1rem",
      fill = FALSE,
      card(
        class = "results-panel",
        full_screen = TRUE,
        card_header(textOutput("results_header", inline = TRUE)),
        card_body(
          class = "results-scroll",
          uiOutput("results")
        )
      ),
      card(
        class = "export-panel",
        card_header(textOutput("export_header", inline = TRUE)),
        card_body(
          class = "export-scroll",
          uiOutput("export_controls")
        )
      )
    ),
    card(
      class = "status-panel-horizontal mt-3",
      full_screen = TRUE,
      card_header(textOutput("status_header", inline = TRUE)),
      card_body(
        class = "status-scroll",
        verbatimTextOutput("status", placeholder = TRUE)
      )
    )
  )
}
