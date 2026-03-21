#' Retrieve recent SEC filing history for a company
#'
#' Returns a data frame listing the most recent EDGAR filings of a specified
#' form type for a given company. Useful for verifying that a company has
#' filed the expected number of 10-K or 10-Q reports, or for retrieving
#' accession numbers needed to access specific filings.
#'
#' @param symbol A character string containing the stock ticker symbol
#'   (e.g. \code{"AAPL"}). Case-insensitive.
#' @param form_type A character string specifying the SEC form type to
#'   retrieve. Common values are \code{"10-K"} (annual report),
#'   \code{"10-Q"} (quarterly report), and \code{"8-K"} (current report).
#'   Defaults to \code{"10-K"}.
#' @param n An integer specifying the maximum number of filings to return.
#'   Defaults to \code{5}.
#'
#' @return A data frame with one row per filing and the following columns:
#' \describe{
#'   \item{symbol}{The ticker symbol passed to the function.}
#'   \item{accession_number}{The SEC accession number uniquely identifying
#'     the filing (e.g. \code{"0000320193-24-000123"}).}
#'   \item{filing_date}{The date the filing was submitted to EDGAR.}
#'   \item{report_date}{The period-end date covered by the filing.}
#'   \item{form}{The form type as recorded in EDGAR.}
#'   \item{primary_document}{The filename of the primary HTML document within
#'     the filing.}
#' }
#'
#' @details
#' Data is retrieved from the SEC EDGAR submissions API
#' (\url{https://data.sec.gov/submissions/}). The API returns up to the 1,000
#' most recent filings across all form types; older filings may not appear.
#'
#' Set your User-Agent once per session:
#' \code{options(edgarfundamentals.user_agent = "Your Name your@@email.com")}
#'
#' @examples
#' \dontrun{
#' options(edgarfundamentals.user_agent = "Jane Smith jane@@example.com")
#'
#' # Five most recent annual reports for Lockheed Martin
#' get_filing_history("LMT", form_type = "10-K", n = 5)
#'
#' # Most recent quarterly reports for Eli Lilly
#' get_filing_history("LLY", form_type = "10-Q", n = 4)
#' }
#'
#' @importFrom httr GET add_headers content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr filter
#' @export
get_filing_history <- function(symbol, form_type = "10-K", n = 5) {

  cik     <- get_cik(symbol)
  cik.pad <- pad_cik(cik)

  response <- httr::GET(
    paste0("https://data.sec.gov/submissions/CIK", cik.pad, ".json"),
    edgar_ua()
  )

  submissions <- httr::content(response, as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()

  Sys.sleep(0.5)

  recent <- submissions$filings$recent

  all.filings <- data.frame(
    accession_number = recent$accessionNumber,
    filing_date      = recent$filingDate,
    report_date      = recent$reportDate,
    form             = recent$form,
    primary_document = recent$primaryDocument,
    stringsAsFactors = FALSE
  )

  filtered <- all.filings |>
    dplyr::filter(.data$form == form_type)

  if (nrow(filtered) == 0) {
    message("No '", form_type, "' filings found for ", symbol,
            " in the most recent EDGAR submissions.")
    return(data.frame())
  }

  out <- utils::head(filtered, n)
  cbind(symbol = symbol, out, row.names = NULL)
}
