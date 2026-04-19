# Runtime checks for R/cache.R and config limits (no network).
# Usage (from repo root): Rscript tests/test_cache_limits_runtime.R

wd <- getwd()
if (basename(wd) == "tests") {
  setwd(dirname(wd))
}

source("R/topics.R", local = FALSE)
source("R/config.R", local = FALSE)
source("R/cache.R", local = FALSE)

stopifnot(identical(default_news_count(), 10L))

k1 <- news_request_cache_key("bra", "2024-01-01", "2024-06-30", c("macro", "budget"), c("a"), 15L, "gpt-5.4")
k2 <- news_request_cache_key("BRA", "2024-01-01", "2024-06-30", c("budget", "macro"), c("a"), 15L, "gpt-5.4")
stopifnot(identical(k1, k2))

k3 <- news_request_cache_key("BRA", "2024-01-01", "2024-06-30", c("budget", "macro"), c("a"), 16L, "gpt-5.4")
stopifnot(!identical(k1, k3))

stopifnot(isTRUE(validate_topic_selection(TRUE, character())$ok))
stopifnot(!isTRUE(validate_topic_selection(FALSE, character())$ok))
stopifnot(isTRUE(validate_topic_selection(FALSE, c("macro", "budget"))$ok))

stopifnot(identical(ui_date_format(), "dd.mm.yyyy"))
stopifnot(identical(as.character(parse_ui_date("19.04.2026")), "2026-04-19"))
stopifnot(identical(format_ui_date("2026-04-19"), "19.04.2026"))

pv_ok <- validate_period_span(as.Date("2024-01-01"), as.Date("2024-12-31"))
stopifnot(isTRUE(pv_ok$ok))

pv_ok_ui <- validate_period_span("01.01.2024", "31.12.2024")
stopifnot(isTRUE(pv_ok_ui$ok))

pv_long <- validate_period_span(as.Date("2020-01-01"), as.Date("2024-12-31"))
stopifnot(!isTRUE(pv_long$ok))

nc_ok <- validate_news_count_input(15L)
stopifnot(isTRUE(nc_ok$ok), nc_ok$n == 15L)

nc_bad <- validate_news_count_input(3L)
stopifnot(!isTRUE(nc_bad$ok))

td <- tempdir()
cd_old <- Sys.getenv("NEWS_CACHE_DIR", unset = "")
Sys.setenv(NEWS_CACHE_DIR = file.path(td, paste0("news_cache_test_", Sys.getpid())))
on.exit(
  {
    if (nzchar(cd_old)) Sys.setenv(NEWS_CACHE_DIR = cd_old) else Sys.unsetenv("NEWS_CACHE_DIR")
  },
  add = TRUE
)

Sys.setenv(NEWS_CACHE_TTL_SEC = "3600")
parsed <- list(country = list(iso3 = "BRA", display_name = "Brazil"), items = list())
news_cache_set("abcfakekey_not_hex_but_ok", parsed, usage = NULL)
stopifnot(!is.null(news_cache_get("abcfakekey_not_hex_but_ok")))

# Force expiry
path <- file.path(news_cache_dir(), "abcfakekey_not_hex_but_ok.rds")
ent <- readRDS(path)
ent$saved_at <- Sys.time() - 7200
saveRDS(ent, path)
stopifnot(is.null(news_cache_get("abcfakekey_not_hex_but_ok")))

message("All cache/limits runtime checks passed.")
