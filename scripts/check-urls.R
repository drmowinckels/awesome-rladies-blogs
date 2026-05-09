library(httr2)
library(jsonlite)
library(here)

source(here::here("scripts", "scrape_helpers.R"))

CONTENT_FIELDS <- list(
  url       = list(image = FALSE, homepage = NULL),
  rss_feed  = list(image = FALSE, homepage = "url"),
  photo_url = list(image = TRUE,  homepage = "url")
)

PACKAGE_FIELDS <- list(
  repo_url        = list(image = FALSE, homepage = NULL),
  pkdown_url      = list(image = FALSE, homepage = NULL),
  bug_reports_url = list(image = FALSE, homepage = NULL),
  logo_url        = list(image = TRUE,  homepage = "pkdown_url")
)

USER_AGENT  <- "rladies-url-checker (+https://github.com/rladies/awesome-rladies-creations)"
TIMEOUT_S   <- 20L
MAX_ACTIVE  <- 8L

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

ensure_scheme <- function(url) {
  if (is.null(url) || is.na(url) || !nzchar(url)) return(NA_character_)
  if (grepl("^https?://", url, ignore.case = TRUE)) return(url)
  paste0("https://", url)
}

build_request <- function(full) {
  request(full) |>
    req_user_agent(USER_AGENT) |>
    req_timeout(TIMEOUT_S) |>
    req_retry(
      max_tries = 2L,
      retry_on_failure = TRUE,
      is_transient = function(resp) resp_status(resp) >= 500L
    ) |>
    req_error(is_error = function(x) FALSE)
}

categorise_resp <- function(resp_or_err, expect_image) {
  if (inherits(resp_or_err, "error")) {
    return(list(status = NA_integer_, content_type = NA_character_,
                error = conditionMessage(resp_or_err), category = "down"))
  }
  status <- resp_status(resp_or_err)
  ctype  <- tryCatch(resp_content_type(resp_or_err), error = function(e) NA_character_)
  category <-
    if (status %in% c(404L, 410L))                                                   "broken"
    else if (status >= 500L)                                                         "down"
    else if (status >= 400L)                                                         "broken"
    else if (expect_image && (length(ctype) == 0 ||
                              !grepl("^image/", ctype, ignore.case = TRUE)))         "not_image"
    else                                                                             "ok"
  list(status = status, content_type = ctype %||% NA_character_,
       error = NA_character_, category = category)
}

check_url <- function(url, expect_image = FALSE) {
  full <- ensure_scheme(url)
  if (is.na(full)) {
    return(list(status = NA_integer_, content_type = NA_character_,
                error = "could not build URL", category = "broken"))
  }
  resp <- tryCatch(req_perform(build_request(full)), error = function(e) e)
  categorise_resp(resp, expect_image)
}

suggest_replacement <- function(homepage_url) {
  if (is_blank(homepage_url)) return(NA_character_)
  html <- fetch_html(ensure_scheme(homepage_url))
  if (is.na(html) || !nzchar(html)) return(NA_character_)
  candidate <- scrape_image(html, homepage_url)
  if (is.na(candidate) || is_blank(candidate)) return(NA_character_)
  if (identical(check_url(candidate, expect_image = TRUE)$category, "ok")) {
    return(candidate)
  }
  NA_character_
}

collect_targets <- function(dir, fields, kind) {
  if (!dir.exists(dir)) return(list())
  files <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
  out <- list()
  for (f in files) {
    entry <- tryCatch(read_json(f), error = function(e) NULL)
    if (is.null(entry)) next
    for (field in names(fields)) {
      cfg <- fields[[field]]
      url <- entry[[field]]
      if (is_blank(url)) next
      out[[length(out) + 1]] <- list(
        kind     = kind,
        file     = basename(f),
        field    = field,
        url      = url,
        full     = ensure_scheme(url),
        image    = isTRUE(cfg$image),
        homepage = if (!is.null(cfg$homepage)) entry[[cfg$homepage]] else NA_character_
      )
    }
  }
  out
}

cat("Collecting URL targets...\n")
targets <- c(
  collect_targets(here("data", "content"),  CONTENT_FIELDS, "content"),
  collect_targets(here("data", "packages"), PACKAGE_FIELDS, "package")
)
cat(sprintf("Found %d URL targets across %d files\n",
            length(targets),
            length(unique(vapply(targets, function(t) t$file, character(1))))))

valid_idx <- which(!is.na(vapply(targets, function(t) t$full, character(1))))
invalid_targets <- targets[setdiff(seq_along(targets), valid_idx)]
valid_targets   <- targets[valid_idx]

cat(sprintf("Checking %d URLs in parallel (max_active=%d)...\n",
            length(valid_targets), MAX_ACTIVE))
reqs <- lapply(valid_targets, function(t) build_request(t$full))
resps <- req_perform_parallel(reqs, on_error = "continue", max_active = MAX_ACTIVE)

rows <- vector("list", length(valid_targets) + length(invalid_targets))
for (i in seq_along(valid_targets)) {
  t <- valid_targets[[i]]
  res <- categorise_resp(resps[[i]], t$image)
  suggestion <- NA_character_
  if (t$image && res$category != "ok" && !is.na(t$homepage)) {
    cat(sprintf("  broken %s in %s — looking for og:image...\n", t$field, t$file))
    suggestion <- suggest_replacement(t$homepage)
  }
  rows[[i]] <- data.frame(
    kind         = t$kind,
    file         = t$file,
    field        = t$field,
    url          = t$url,
    status       = res$status %||% NA_integer_,
    content_type = res$content_type %||% NA_character_,
    category     = res$category,
    error        = res$error %||% NA_character_,
    suggestion   = suggestion %||% NA_character_,
    stringsAsFactors = FALSE
  )
}
for (j in seq_along(invalid_targets)) {
  t <- invalid_targets[[j]]
  rows[[length(valid_targets) + j]] <- data.frame(
    kind         = t$kind,
    file         = t$file,
    field        = t$field,
    url          = t$url,
    status       = NA_integer_,
    content_type = NA_character_,
    category     = "broken",
    error        = "could not build URL",
    suggestion   = NA_character_,
    stringsAsFactors = FALSE
  )
}

results <- do.call(rbind, rows)
results <- results[order(results$category, results$kind, results$file, results$field), ]

out_path <- "url-check-report.tsv"
write.table(results, out_path, sep = "\t", row.names = FALSE, quote = FALSE, na = "")

summary_tbl <- table(factor(results$category, levels = c("broken", "not_image", "down", "ok")))
cat("Summary:\n")
for (n in names(summary_tbl)) {
  cat(sprintf("  %s: %d\n", n, summary_tbl[[n]]))
}
cat(sprintf("Wrote %s\n", out_path))
