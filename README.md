# edgarfundamentals

**Retrieve Fundamental Financial Data from SEC EDGAR**

`edgarfundamentals` provides a simple, ticker-based interface for retrieving
key fundamental financial ratios directly from SEC EDGAR 10-K filings. No API
key or paid subscription is required.

## Installation

```r
# From CRAN (once published)
install.packages("edgarfundamentals")

# Development version
devtools::install_github("rschumaker/edgarfundamentals")
```

## Setup

The SEC requests that automated tools identify themselves via a User-Agent
header. Set this once per session:

```r
options(edgarfundamentals.user_agent = "Your Name your@email.com")
```

## Functions

| Function | Description |
|----------|-------------|
| `get_cik(symbol)` | Translate a ticker to its SEC Central Index Key |
| `get_fundamentals(symbol, to_date)` | Key ratios for one stock from its most recent 10-K |
| `get_fundamentals_batch(symbols, to_date)` | Key ratios for a portfolio of stocks |
| `get_filing_history(symbol, form_type, n)` | Recent SEC filing history for a stock |

## Quick Example

```r
library(edgarfundamentals)
options(edgarfundamentals.user_agent = "Jane Smith jane@example.com")

# Single stock
get_fundamentals("LLY", to_date = "2024-12-31")

# Portfolio
healthcare <- c("UNH", "PFE", "MRK", "ABT", "LLY", "CVS", "AMGN")
get_fundamentals_batch(healthcare, to_date = "2024-12-31")
```

## Notes for Building and Submission

After cloning, generate documentation and check the package before submission:

```r
devtools::document()   # generates man/ files from roxygen2 comments
devtools::check()      # runs R CMD check -- must pass with 0 errors, 0 warnings
devtools::build()      # builds the .tar.gz for CRAN submission
```

The `man/` directory is generated automatically by `devtools::document()` and
is not tracked in version control.

## Data Source

All financial statement data comes from the SEC EDGAR XBRL API
(`data.sec.gov`), specifically the `companyfacts` endpoint. Ratios reflect
the most recently completed fiscal year from 10-K filings. The PE ratio
additionally uses current market prices from Yahoo Finance via `tidyquant`.

## License

MIT © Robert P Schumaker
