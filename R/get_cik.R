#' Look up a company's SEC Central Index Key from its ticker symbol
#'
#' Translates a stock ticker symbol into the corresponding SEC EDGAR Central
#' Index Key (CIK). The CIK is a unique numerical identifier assigned by the
#' SEC to every company that files with EDGAR. It is required for all
#' subsequent EDGAR API calls.
#'
#' @param symbol A character string containing the stock ticker symbol
#'   (e.g. \code{"AAPL"}). Case-insensitive.
#'
#' @return A character string containing the CIK number (without zero-padding).
#'
#' @details
#' Data is retrieved from \url{https://www.sec.gov/files/company_tickers.json},
#' a publicly maintained mapping file updated by the SEC. No API key is
#' required.
#'
#' The SEC requests that automated tools identify themselves via a User-Agent
#' header. Set your identifier once per session with:
#' \code{options(edgarfundamentals.user_agent = "Your Name your@@email.com")}
#'
#' @examples
#' \dontrun{
#' options(edgarfundamentals.user_agent = "Jane Smith jane@@example.com")
#' get_cik("AAPL")
#' get_cik("LLY")
#' }
#'
#' @importFrom httr GET add_headers content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr filter pull
#' @export
get_cik <- function(symbol) {
  tickers.raw <- httr::GET(
    "https://www.sec.gov/files/company_tickers.json",
    edgar_ua()
  ) |>
    httr::content(as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()

  tickers.df <- do.call(rbind, lapply(tickers.raw, as.data.frame))

  match.row <- tickers.df[toupper(tickers.df$ticker) == toupper(symbol), ]

  if (nrow(match.row) == 0) {
    stop("Ticker '", symbol, "' not found in SEC EDGAR. ",
         "Verify the symbol is a US-listed company.")
  }

  as.character(match.row$cik_str[1])
}
