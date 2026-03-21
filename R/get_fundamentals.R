#' Retrieve key fundamental financial ratios for a single stock
#'
#' Pulls fundamental financial data directly from a company's most recent
#' annual 10-K filing in SEC EDGAR and computes five key ratios. No API key
#' or paid subscription is required.
#'
#' @param symbol A character string containing the stock ticker symbol
#'   (e.g. \code{"LLY"}). Case-insensitive.
#' @param to_date A character string in \code{"YYYY-MM-DD"} format. The
#'   function returns ratios from the most recent 10-K with a period end date
#'   on or before this date. Defaults to today's date.
#'
#' @return A named numeric vector with the following elements:
#' \describe{
#'   \item{CIK}{SEC Central Index Key -- the company's unique EDGAR identifier.}
#'   \item{EPS}{Diluted Earnings Per Share (USD per share) from the most recent
#'     qualifying 10-K. Falls back to basic EPS if diluted is unavailable.}
#'   \item{NetIncome}{Net income attributable to the company (USD).}
#'   \item{ROE}{Return on Equity as a percentage (NetIncome / StockholdersEquity
#'     * 100). A measure of how efficiently the company generates profit from
#'     shareholder capital.}
#'   \item{DE}{Debt-to-Equity ratio (LongTermDebt / StockholdersEquity). A
#'     measure of financial leverage.}
#'   \item{PE}{Price-to-Earnings ratio, computed by dividing the most recent
#'     adjusted closing price (from Yahoo Finance via tidyquant) by EPS. Returns
#'     \code{NA} if EPS is zero or negative.}
#' }
#'
#' @details
#' Financial statement values are extracted from XBRL-tagged 10-K filings via
#' the SEC EDGAR companyfacts API
#' (\url{https://data.sec.gov/api/xbrl/companyfacts/}). Because data comes
#' from annual filings, ratios reflect the most recently completed fiscal year
#' ending on or before \code{to_date}, not real-time values.
#'
#' Fallback XBRL tags are attempted automatically when a company uses a
#' non-standard tag name for a concept. A courtesy pause of 0.5 seconds is
#' inserted after the companyfacts API call to respect the SEC's rate limit
#' of 10 requests per second.
#'
#' Set your User-Agent once per session:
#' \code{options(edgarfundamentals.user_agent = "Your Name your@@email.com")}
#'
#' @examples
#' \dontrun{
#' options(edgarfundamentals.user_agent = "Jane Smith jane@@example.com")
#'
#' # Fundamentals for Eli Lilly as of end of 2024
#' get_fundamentals("LLY", "2024-12-31")
#'
#' # Fundamentals as of today
#' get_fundamentals("JNJ")
#' }
#'
#' @importFrom httr GET add_headers content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr filter arrange desc slice pull
#' @importFrom tidyquant tq_get
#' @export
get_fundamentals <- function(symbol, to_date = as.character(Sys.Date())) {

  cik     <- get_cik(symbol)
  cik.pad <- pad_cik(cik)

  facts <- httr::GET(
    paste0("https://data.sec.gov/api/xbrl/companyfacts/CIK", cik.pad, ".json"),
    edgar_ua()
  ) |>
    httr::content(as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()

  Sys.sleep(0.5)

  # Extract raw values -- each concept tries a primary tag then fallback tags
  eps        <- latest_10k(facts, "EarningsPerShareDiluted",
                           fallback_tags = "EarningsPerShareBasic",
                           unit = "USD/shares", to_date = to_date)

  net.income <- latest_10k(facts, "NetIncomeLoss",
                           fallback_tags = c("ProfitLoss", "NetIncome"),
                           unit = "USD", to_date = to_date)

  equity     <- latest_10k(facts, "StockholdersEquity",
                           fallback_tags = "StockholdersEquityIncludingPortionAttributableToNoncontrollingInterest",
                           unit = "USD", to_date = to_date)

  debt       <- latest_10k(facts, "LongTermDebt",
                           fallback_tags = c("LongTermDebtNoncurrent", "LongTermNotesPayable"),
                           unit = "USD", to_date = to_date)

  # Compute derived ratios
  roe <- if (!is.na(net.income) && !is.na(equity) && equity != 0)
    round(net.income / equity * 100, 2) else NA_real_

  de  <- if (!is.na(debt) && !is.na(equity) && equity != 0)
    round(debt / equity, 2) else NA_real_

  # P/E uses the most recent adjusted price from tidyquant
  prices <- suppressMessages(
    tidyquant::tq_get(symbol,
                      from = as.character(as.Date(to_date) - 7),
                      to   = as.character(to_date))
  )
  price <- if (nrow(prices) > 0) prices$adjusted[nrow(prices)] else NA_real_
  pe    <- if (!is.na(eps) && eps > 0) round(price / eps, 2) else NA_real_

  c(CIK       = as.numeric(cik),
    EPS       = round(eps,        2),
    NetIncome = round(net.income, 0),
    ROE       = roe,
    DE        = de,
    PE        = pe)
}
