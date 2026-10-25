#' @importFrom rlang .data
NULL
# Internal helpers -- not exported

# Session cache for the SEC ticker file and companyfacts downloads.
.edgar_cache <- new.env(parent = emptyenv())

# Retrieve and validate the User-Agent string from options.
# SEC policy requires requests to identify the requester with a contact email;
# requests without one are frequently refused with HTTP 403.
#   options(edgarfundamentals.user_agent = "Your Name your@email.com")
edgar_ua <- function() {
  ua <- getOption("edgarfundamentals.user_agent")
  if (is.null(ua)) {
    if (is.null(.edgar_cache$ua_warned)) {
      warning("edgarfundamentals.user_agent is not set; the SEC may refuse ",
              "requests without a contact email. Set it with ",
              "options(edgarfundamentals.user_agent = \"Your Name your@email.com\")",
              call. = FALSE)
      .edgar_cache$ua_warned <- TRUE
    }
    ua <- "edgarfundamentals R package https://cran.r-project.org/package=edgarfundamentals"
  }
  httr::add_headers("User-Agent" = ua)
}

# GET a JSON endpoint from the SEC, failing with a readable message on HTTP errors
# (otherwise a 403 or 404 HTML page reaches fromJSON and fails cryptically).
edgar_get_json <- function(url) {
  resp <- httr::GET(url, edgar_ua())
  status <- httr::status_code(resp)
  if (status != 200) {
    hint <- switch(as.character(status),
      "403" = " -- the SEC refused the request; check your User-Agent option or slow down.",
      "404" = " -- not found; the company may have no XBRL financial data.",
      "429" = " -- rate limited by the SEC; wait and retry.",
      "")
    stop("SEC request failed (HTTP ", status, ") for ", url, hint, call. = FALSE)
  }
  httr::content(resp, as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()
}

# Zero-pad a CIK to 10 digits as required by EDGAR URL format.
pad_cik <- function(cik) {
  formatC(as.integer(cik), width = 10, flag = "0")
}

# Annual report forms whose facts count as fiscal-year data.
ANNUAL_FORMS <- c("10-K", "10-K/A", "10-KT", "10-KT/A")

# A fiscal year of 52/53 weeks or 12 months is 357-378 days long.
ANNUAL_DAYS <- c(350, 380)

# Facts for one tag, restricted to annual-report forms filed on or before to_date.
# Duration concepts (flows) are further restricted to fiscal-year-length periods,
# because 10-Ks also carry fourth-quarter and prior-period comparatives that share
# the fiscal-year end date.
annual_rows <- function(facts, tag, unit, to_date, duration, ns = "us-gaap") {
  rows <- facts$facts[[ns]][[tag]]$units[[unit]]
  if (is.null(rows)) return(NULL)
  df <- as.data.frame(rows)
  if (nrow(df) == 0 || !all(c("end", "val", "filed", "form") %in% names(df))) return(NULL)
  df <- df[df$form %in% ANNUAL_FORMS & as.Date(df$filed) <= as.Date(to_date), , drop = FALSE]
  if (duration) {
    if (!"start" %in% names(df)) return(NULL)
    days <- as.numeric(as.Date(df$end) - as.Date(df$start))
    df <- df[!is.na(days) & days >= ANNUAL_DAYS[1] & days <= ANNUAL_DAYS[2], , drop = FALSE]
  }
  if (nrow(df) == 0) return(NULL)
  df
}

# The end date of the most recent fiscal year whose 10-K was filed on or before
# to_date. Every ratio is then computed from that one fiscal year, so values from
# different years (a stale tag, or a concept the company stopped reporting) are
# never mixed.
fiscal_year_end <- function(facts, to_date) {
  anchors <- list(
    c("NetIncomeLoss", "USD"),
    c("ProfitLoss", "USD"),
    c("Revenues", "USD"),
    c("RevenueFromContractWithCustomerExcludingAssessedTax", "USD"),
    c("EarningsPerShareDiluted", "USD/shares"),
    c("EarningsPerShareBasic", "USD/shares")
  )
  ends <- unlist(lapply(anchors, function(a) {
    df <- annual_rows(facts, a[1], a[2], to_date, duration = TRUE)
    if (is.null(df)) NULL else as.character(max(as.Date(df$end)))
  }))
  if (length(ends) == 0) return(as.Date(NA))
  max(as.Date(ends))
}

# Value of one concept for the fiscal year ending at fy_end, as known on to_date.
# Tries each tag in order and returns the first with a fact for that fiscal year,
# so a primary tag the company has stopped using falls through to its replacement
# instead of returning a years-old value. Within a tag the latest filing on or
# before to_date wins (a restatement that was public by then is legitimate
# point-in-time data). Returns list(val, filed) or list(NA, NA).
fy_value <- function(facts, tags, unit, to_date, fy_end, duration,
                     ns = "us-gaap", tolerance = 14) {
  none <- list(val = NA_real_, filed = as.Date(NA))
  if (is.na(fy_end)) return(none)
  for (tag in tags) {
    df <- annual_rows(facts, tag, unit, to_date, duration, ns)
    if (is.null(df)) next
    df <- df[abs(as.numeric(as.Date(df$end) - fy_end)) <= tolerance & !is.na(df$val), , drop = FALSE]
    if (nrow(df) == 0) next
    df <- df[order(as.Date(df$filed), decreasing = TRUE), , drop = FALSE]
    return(list(val = as.numeric(df$val[1]), filed = as.Date(df$filed[1])))
  }
  none
}

# Shares outstanding for the fiscal year. Prefers the balance-sheet count; falls
# back to the cover-page count (dei namespace), which is dated shortly after the
# fiscal year ends.
fy_shares <- function(facts, to_date, fy_end) {
  bs <- fy_value(facts, c("CommonStockSharesOutstanding",
                          "CommonStockSharesIssuedAndOutstanding"),
                 "shares", to_date, fy_end, duration = FALSE)
  if (!is.na(bs$val)) return(bs)
  df <- annual_rows(facts, "EntityCommonStockSharesOutstanding", "shares",
                    to_date, duration = FALSE, ns = "dei")
  if (is.null(df) || is.na(fy_end)) return(list(val = NA_real_, filed = as.Date(NA)))
  lag <- as.numeric(as.Date(df$end) - fy_end)
  df <- df[lag >= 0 & lag <= 150, , drop = FALSE]
  if (nrow(df) == 0) return(list(val = NA_real_, filed = as.Date(NA)))
  # Multi-class issuers report one row per class on the same date: sum them.
  latest <- max(as.Date(df$filed))
  df <- df[as.Date(df$filed) == latest, , drop = FALSE]
  df <- df[as.Date(df$end) == max(as.Date(df$end)), , drop = FALSE]
  list(val = sum(as.numeric(df$val)), filed = latest)
}

# Closing price on or just before to_date, in the share basis of that date.
# Yahoo prices are retroactively split-adjusted for every later split, which would
# put a pre-split EPS against a post-split price, so later splits are undone.
# Unadjusted close is used rather than the dividend-adjusted price, which
# understates historical prices.
price_on <- function(symbol, to_date) {
  prices <- tryCatch(
    suppressWarnings(suppressMessages(
      tidyquant::tq_get(symbol,
                        from = as.character(as.Date(to_date) - 10),
                        to   = as.character(as.Date(to_date) + 1))
    )),
    error = function(e) NULL)
  if (is.null(prices) || !is.data.frame(prices) || nrow(prices) == 0) return(NA_real_)
  prices <- prices[as.Date(prices$date) <= as.Date(to_date), , drop = FALSE]
  if (nrow(prices) == 0) return(NA_real_)
  close <- prices$close[nrow(prices)]
  close / split_factor(symbol, after = to_date)
}

# Cumulative split ratio for splits after `after` (and on or before `upto`),
# using quantmod's convention: a 10-for-1 split is 0.1. Returns 1 if none.
split_factor <- function(symbol, after, upto = Sys.Date()) {
  key <- paste0("splits_", toupper(symbol))
  spl <- .edgar_cache[[key]]
  if (is.null(spl)) {
    spl <- tryCatch(
      suppressWarnings(suppressMessages(
        tidyquant::tq_get(symbol, get = "splits", from = "1900-01-01")
      )),
      error = function(e) NULL)
    if (!is.data.frame(spl)) spl <- data.frame(date = as.Date(character()), value = numeric())
    .edgar_cache[[key]] <- spl
  }
  if (nrow(spl) == 0) return(1)
  d <- as.Date(spl$date)
  keep <- d > as.Date(after) & d <= as.Date(upto) & !is.na(spl$value)
  if (!any(keep)) return(1)
  prod(spl$value[keep])
}
