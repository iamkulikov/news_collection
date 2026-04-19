# Sovereign risk news — Shiny entry point
# Dependencies: shiny, bslib

library(shiny)
library(bslib)
library(jsonlite)

source("R/countries.R", local = FALSE)
source("R/topics.R", local = FALSE)
source("R/config.R", local = FALSE)
source("R/cache.R", local = FALSE)
source("R/openai_client.R", local = FALSE)
source("R/prompt.R", local = FALSE)
source("R/schema.R", local = FALSE)
source("R/news_openai.R", local = FALSE)
source("R/i18n.R", local = FALSE)
source("R/render.R", local = FALSE)
source("R/followup.R", local = FALSE)
source("R/app_ui.R", local = FALSE)
source("R/app_server.R", local = FALSE)

shinyApp(ui = app_ui, server = app_server)
