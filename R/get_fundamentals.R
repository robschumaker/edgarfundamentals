#' Retrieve key fundamental financial ratios for a single stock
#'
#' Pulls fundamental financial data directly from a company's most recent
#' annual 10-K filing in SEC EDGAR and computes key financial ratios. No API
#' key or paid subscription is required.
#'
#' @param symbol A character string containing the stock ticker symbol
#'   (e.g. \code{"LLY"}). Case-insensitive.
#' @param to_date A character string in \code{"YYYY-MM-DD"} format. The
#'   function returns ratios from the most recent 10-K that had been
#'   \emph{filed} on or before this date, so results contain only information
#'   that was public on that date. Defaults to today's date.
#'
#' @return A named numeric vector with the following elements:
#' \describe{
#'   \item{CIK}{SEC Central Index Key -- the company's unique EDGAR identifier.}
#'   \item{EPS}{Diluted Earnings Per Share (USD per share) from the most recent
#'     qualifying 10-K. Falls back to basic EPS if diluted is unavailable.}
#'   \item{NetIncome}{Net income attributable to the company (USD).}
#'   \item{Revenue}{Total revenue (USD).}
#'   \item{ROE}{Return on Equity as a percentage (NetIncome / StockholdersEquity
#'     * 100). A measure of how efficiently the company generates profit from
#'     shareholder capital.}
#'   \item{ROA}{Return on Assets as a percentage (NetIncome / TotalAssets
#'     * 100). A measure of how efficiently the company uses its assets.}
#'   \item{DE}{Debt-to-Equity ratio (long-term debt including its current
#'     portion / StockholdersEquity). A
#'     measure of financial leverage.}
#'   \item{CurrentRatio}{Current Assets divided by Current Liabilities. A
#'     measure of short-term liquidity; values above 1 indicate the company
#'     can cover near-term obligations.}
#'   \item{GrossMargin}{Gross Profit (or Revenue minus Cost of Revenue when
#'     gross profit is not tagged) as a percentage of Revenue. Measures
#'     pricing power and production efficiency.}
#'   \item{OperatingMargin}{Operating Income as a percentage of Revenue.
#'     Measures operational efficiency before interest and taxes.}
#'   \item{NetMargin}{Net Income as a percentage of Revenue. Bottom-line
#'     profitability after all expenses.}
#'   \item{PE}{Price-to-Earnings ratio, computed by dividing the closing price
#'     on or before \code{to_date} (from Yahoo Finance via tidyquant) by EPS.
#'     Returns \code{NA} if EPS is zero or negative.}
#'   \item{PB}{Price-to-Book ratio, computed by dividing the closing price by
#'     Book Value per Share (StockholdersEquity / SharesOutstanding). Returns
#'     \code{NA} if shares outstanding is unavailable or equity is negative.}
#'   \item{DIV}{Dividend Yield as a percentage (DividendsPerShareDeclared /
#'     Price * 100). Returns \code{NA} if no dividend data is available.}
#' }
#'
#' @details
#' Financial statement values are extracted from XBRL-tagged 10-K filings via
#' the SEC EDGAR companyfacts API
#' (\url{https://data.sec.gov/api/xbrl/companyfacts/}). Results are point in
#' time: the function finds the most recent fiscal year whose 10-K was filed
#' on or before \code{to_date}, and every value comes from that one fiscal
#' year, as reported in filings made on or before \code{to_date}. A fiscal
#' year that has ended but whose 10-K was not yet filed is not used.
#'
#' Fallback XBRL tags are attempted automatically when a company uses a
#' non-standard tag name for a concept, or has switched tags over time. A
#' concept with no fact for the selected fiscal year is \code{NA} rather than
#' a value from an earlier year.
#'
#' Prices are the unadjusted close on or before \code{to_date}. Per-share
#' values are put on the same share basis as that price when a stock split
#' occurred between the 10-K filing and \code{to_date}. A courtesy pause of 0.5 seconds is
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
#' @importFrom httr GET add_headers content status_code
#' @importFrom jsonlite fromJSON
#' @importFrom tidyquant tq_get
#' @export
get_fundamentals <- function(symbol, to_date = as.character(Sys.Date())) {

  to_date <- as.character(as.Date(to_date))
  cik     <- get_cik(symbol)
  cik.pad <- pad_cik(cik)

  facts <- edgar_get_json(
    paste0("https://data.sec.gov/api/xbrl/companyfacts/CIK", cik.pad, ".json"))

  Sys.sleep(0.5)

  fy <- fiscal_year_end(facts, to_date)
  if (is.na(fy)) {
    warning("No 10-K filed on or before ", to_date, " for ", symbol, call. = FALSE)
  }

  flow  <- function(tags, unit = "USD") fy_value(facts, tags, unit, to_date, fy, duration = TRUE)
  stock <- function(tags, unit = "USD") fy_value(facts, tags, unit, to_date, fy, duration = FALSE)

  # Each concept tries a primary tag then fallback tags, all for the same fiscal year
  eps.f      <- flow(c("EarningsPerShareDiluted", "EarningsPerShareBasic"), "USD/shares")
  net.income <- flow(c("NetIncomeLoss", "ProfitLoss"))$val
  # Revenues (the total) before contract revenue, which excludes items such as
  # alliance and royalty income (PFE reports both and they differ by billions).
  revenue    <- flow(c("Revenues",
                       "RevenueFromContractWithCustomerExcludingAssessedTax",
                       "SalesRevenueNet", "SalesRevenueGoodsNet",
                       "RevenueFromContractWithCustomerIncludingAssessedTax"))$val
  equity     <- stock(c("StockholdersEquity",
                        "StockholdersEquityIncludingPortionAttributableToNoncontrollingInterest"))$val

  # Total long-term debt including its current portion. Many filers stopped tagging
  # LongTermDebt and report the noncurrent and current pieces separately.
  debt <- stock(c("LongTermDebt",
                  "LongTermDebtAndCapitalLeaseObligationsIncludingCurrentMaturities"))$val
  if (is.na(debt)) {
    noncurrent <- stock(c("LongTermDebtNoncurrent", "LongTermDebtAndCapitalLeaseObligations"))$val
    current    <- stock(c("LongTermDebtCurrent", "LongTermDebtAndCapitalLeaseObligationsCurrent"))$val
    debt <- if (!is.na(noncurrent)) noncurrent + ifelse(is.na(current), 0, current) else NA_real_
  }
  if (is.na(debt)) debt <- stock("LongTermNotesPayable")$val

  assets         <- stock("Assets")$val
  current.assets <- stock("AssetsCurrent")$val
  current.liab   <- stock("LiabilitiesCurrent")$val
  gross.profit   <- flow("GrossProfit")$val
  if (is.na(gross.profit) && !is.na(revenue)) {
    # Many filers (e.g. LLY, PFE) never tag GrossProfit; derive it from cost of revenue.
    cost <- flow(c("CostOfGoodsAndServicesSold", "CostOfRevenue", "CostOfGoodsSold"))$val
    if (!is.na(cost)) gross.profit <- revenue - cost
  }
  operating.inc  <- flow("OperatingIncomeLoss")$val
  shares.f       <- fy_shares(facts, to_date, fy)
  div.f          <- flow(c("CommonStockDividendsPerShareDeclared",
                           "CommonStockDividendsPerShareCashPaid"), "USD/shares")

  # Put per-share figures on the share basis of to_date: undo nothing for splits
  # before the 10-K was filed (filings restate those), adjust for splits after it.
  basis <- function(f) if (is.na(f$filed)) 1 else split_factor(symbol, f$filed, to_date)
  eps           <- eps.f$val * basis(eps.f)
  div.per.share <- div.f$val * basis(div.f)
  shares.out    <- shares.f$val / basis(shares.f)

  # Compute derived ratios
  roe <- if (!is.na(net.income) && !is.na(equity) && equity != 0)
    round(net.income / equity * 100, 2) else NA_real_

  roa <- if (!is.na(net.income) && !is.na(assets) && assets != 0)
    round(net.income / assets * 100, 2) else NA_real_

  de  <- if (!is.na(debt) && !is.na(equity) && equity != 0)
    round(debt / equity, 2) else NA_real_

  current.ratio <- if (!is.na(current.assets) && !is.na(current.liab) && current.liab != 0)
    round(current.assets / current.liab, 2) else NA_real_

  gross.margin <- if (!is.na(gross.profit) && !is.na(revenue) && revenue != 0)
    round(gross.profit / revenue * 100, 2) else NA_real_

  operating.margin <- if (!is.na(operating.inc) && !is.na(revenue) && revenue != 0)
    round(operating.inc / revenue * 100, 2) else NA_real_

  net.margin <- if (!is.na(net.income) && !is.na(revenue) && revenue != 0)
    round(net.income / revenue * 100, 2) else NA_real_

  # PE, PB, and DIV use the closing price on or just before to_date
  price <- price_on(symbol, to_date)

  pe  <- if (!is.na(eps) && eps > 0 && !is.na(price)) round(price / eps, 2) else NA_real_

  pb  <- if (!is.na(shares.out) && shares.out > 0 && !is.na(equity) && equity > 0 && !is.na(price))
    round(price / (equity / shares.out), 2) else NA_real_

  div <- if (!is.na(div.per.share) && !is.na(price) && price > 0)
    round(div.per.share / price * 100, 2) else NA_real_

  c(CIK            = as.numeric(cik),
    EPS            = round(eps,        2),
    NetIncome      = round(net.income, 0),
    Revenue        = round(revenue,    0),
    ROE            = roe,
    ROA            = roa,
    DE             = de,
    CurrentRatio   = current.ratio,
    GrossMargin    = gross.margin,
    OperatingMargin = operating.margin,
    NetMargin      = net.margin,
    PE             = pe,
    PB             = pb,
    DIV            = div)
}
