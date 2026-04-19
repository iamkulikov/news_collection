# Runtime checks for R/followup.R
# Usage (from repo root): Rscript tests/test_followup_runtime.R
# Or: cd tests && Rscript test_followup_runtime.R

wd <- getwd()
if (basename(wd) == "tests") {
  setwd(dirname(wd))
}

source("R/countries.R", local = FALSE)
source("R/topics.R", local = FALSE)
source("R/config.R", local = FALSE)
source("R/followup.R", local = FALSE)

stopifnot(identical(parse_followup_text(NULL), character()))
stopifnot(identical(parse_followup_text(""), character()))
ws_only <- paste0("  ", "\n", "  ")
stopifnot(identical(parse_followup_text(ws_only), character()))
stopifnot(identical(parse_followup_text("a;b"), c("a", "b")))
stopifnot(identical(parse_followup_text(paste0("a", "\n", "b")), c("a", "b")))
stopifnot(identical(parse_followup_text("  x  y  ; z "), c("x y", "z")))
stopifnot(identical(parse_followup_text("dup;dup"), c("dup")))

v_ok <- validate_followup_topics(letters[1:3])
stopifnot(isTRUE(v_ok$ok), length(v_ok$errors) == 0L)

too_many <- validate_followup_topics(letters[1:(max_followup_lines + 1L)])
stopifnot(!isTRUE(too_many$ok))

long <- c("ok", strrep("a", max_followup_chars + 1L))
v_long <- validate_followup_topics(long)
stopifnot(!isTRUE(v_long$ok))

p_off <- process_followup_input(FALSE, "should ignore")
stopifnot(p_off$ok, length(p_off$topics) == 0L)

p_on <- process_followup_input(TRUE, "one; two")
stopifnot(p_on$ok, identical(p_on$topics, c("one", "two")))

p_empty <- process_followup_input(TRUE, "   ")
stopifnot(!p_empty$ok)

p_11 <- process_followup_input(TRUE, paste(letters[1:11], collapse = ";"))
stopifnot(!p_11$ok)

message("All followup runtime checks passed.")
