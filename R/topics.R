# Six thematic blocks — must match the base prompt (macro through IFI/research).
# Shiny shows `names` in the UI and passes `values` to input$topics (stable ids for prompts/JSON).

topic_choices <- c(
  "Macroeconomics" = "macro",
  "Budget & fiscal policy" = "budget",
  "Public sector & implicit liabilities" = "public_sector",
  "External position" = "external",
  "Institutions, human capital & cross-border relations" = "institutions",
  "Economic research, Article IV IMF & IFI country reviews" = "research_ifis"
)

# Values passed to the server / prompts (same order as `topic_choices`).
all_topic_ids <- unname(topic_choices)

#' Require at least one topic when the user is not searching all topics.
#'
#' @param all_topics Logical from `input$all_topics`.
#' @param topic_ids Character vector of selected topic ids (may be empty).
#' @return `list(ok = logical, errors = character())`.
validate_topic_selection <- function(all_topics, topic_ids) {
  if (isTRUE(all_topics)) {
    return(list(ok = TRUE, errors = character()))
  }
  ids <- topic_ids
  if (is.null(ids) || !length(ids) || !any(nzchar(ids))) {
    return(list(
      ok = FALSE,
      errors = paste0(
        "No topics selected. Choose one or more topics under Topics, ",
        "or enable «Search all topics»."
      )
    ))
  }
  list(ok = TRUE, errors = character())
}
