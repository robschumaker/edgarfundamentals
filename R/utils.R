#' @importFrom rlang .data
NULL
# Internal helpers -- not exported

# Retrieve and validate the User-Agent string from options.
# SEC policy requires requests to identify the requester.
# Users should set this once per session:
#   options(edgarfundamentals.user_agent = "Your Name your@email.com")
edgar_ua <- function() {
  ua <- getOption("edgarfundamentals.user_agent",
                  default = "edgarfundamentals R package https://cran.r-project.org/package=edgarfundamentals")
  httr::add_headers("User-Agent" = ua)
}

# Zero-pad a CIK to 10 digits as required by EDGAR URL format.
pad_cik <- function(cik) {
  formatC(as.integer(cik), width = 10, flag = "0")
}

# Extract the most recent 10-K value for a single XBRL concept.
# Tries each tag in order and returns the first non-NA result.
# primary_tag  -- the preferred us-gaap tag name (e.g. "EarningsPerShareDiluted")
# fallback_tags -- character vector of alternative tag names to try if primary fails
# unit          -- XBRL unit string (e.g. "USD/shares" or "USD")
# to_date       -- only consider filings with period end on or before this date
latest_10k <- function(facts, primary_tag, fallback_tags = NULL, unit, to_date) {
  for (tag in c(primary_tag, fallback_tags)) {
    result <- tryCatch({
      df <- as.data.frame(facts$facts$`us-gaap`[[tag]]$units[[unit]])
      if (nrow(df) == 0) next
      val <- df |>
        dplyr::filter(.data$form == "10-K", as.Date(.data$end) <= as.Date(to_date)) |>
        dplyr::arrange(dplyr::desc(.data$end), dplyr::desc(.data$filed)) |>
        dplyr::slice(1) |>
        dplyr::pull(.data$val)
      if (length(val) > 0 && !is.na(val)) return(as.numeric(val))
      NA_real_
    }, error = function(e) NA_real_)
    if (!is.na(result)) return(result)
  }
  NA_real_
}
