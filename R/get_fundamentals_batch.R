#' Retrieve fundamental financial ratios for a portfolio of stocks
#'
#' A vectorized wrapper around \code{\link{get_fundamentals}} that accepts a
#' vector of ticker symbols and returns a tidy data frame with one row per
#' stock. Failed lookups are recorded as \code{NA} rather than stopping
#' execution, so a single problematic ticker does not interrupt the batch.
#'
#' @param symbols A character vector of stock ticker symbols
#'   (e.g. \code{c("LLY", "PFE", "UNH")}). Case-insensitive.
#' @param to_date A character string in \code{"YYYY-MM-DD"} format. Passed
#'   to \code{\link{get_fundamentals}} for each symbol. Defaults to today's
#'   date.
#'
#' @return A data frame with one row per symbol and the following columns:
#'   \code{symbol}, \code{CIK}, \code{EPS}, \code{NetIncome}, \code{Revenue},
#'   \code{ROE}, \code{ROA}, \code{DE}, \code{CurrentRatio},
#'   \code{GrossMargin}, \code{OperatingMargin}, \code{NetMargin},
#'   \code{PE}, \code{PB}, \code{DIV}. See \code{\link{get_fundamentals}}
#'   for definitions. Rows where data retrieval failed contain \code{NA}
#'   for all ratio columns.
#'
#' @details
#' Each symbol requires two SEC EDGAR API calls (one for the CIK lookup and
#' one for the companyfacts data) plus one Yahoo Finance call for the current
#' price. A 0.5-second pause is inserted after each companyfacts call to
#' respect the SEC rate limit of 10 requests per second. For a portfolio of
#' 14 stocks, expect a total retrieval time of approximately 20--30 seconds.
#'
#' Set your User-Agent once per session:
#' \code{options(edgarfundamentals.user_agent = "Your Name your@@email.com")}
#'
#' @examples
#' \dontrun{
#' options(edgarfundamentals.user_agent = "Jane Smith jane@@example.com")
#'
#' healthcare <- c("UNH", "PFE", "MRK", "ABT", "LLY", "CVS", "AMGN")
#' get_fundamentals_batch(healthcare, "2024-12-31")
#'
#' defense <- c("LMT", "RTX", "NOC", "GD", "HII", "LHX", "LDOS")
#' get_fundamentals_batch(defense, "2024-12-31")
#' }
#'
#' @seealso \code{\link{get_fundamentals}} for single-stock retrieval.
#' @export
get_fundamentals_batch <- function(symbols, to_date = as.character(Sys.Date())) {

  na_row <- c(CIK             = NA_real_, EPS             = NA_real_,
              NetIncome       = NA_real_, Revenue         = NA_real_,
              ROE             = NA_real_, ROA             = NA_real_,
              DE              = NA_real_, CurrentRatio    = NA_real_,
              GrossMargin     = NA_real_, OperatingMargin = NA_real_,
              NetMargin       = NA_real_, PE              = NA_real_,
              PB              = NA_real_, DIV             = NA_real_)

  results <- lapply(symbols, function(sym) {
    tryCatch(
      get_fundamentals(sym, to_date),
      error = function(e) {
        message("edgarfundamentals: failed for ", sym, " -- ", conditionMessage(e))
        na_row
      }
    )
  })

  out <- as.data.frame(do.call(rbind, results))
  out <- cbind(symbol = symbols, out, row.names = NULL)
  out
}
