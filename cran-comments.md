## R CMD check results

0 errors | 0 warnings | 0 notes

## Comments to CRAN

This is an update (0.1.2 -> 0.1.3). It corrects look-ahead bias in
`get_fundamentals()`: `to_date` now selects the latest 10-K *filed* on or
before that date, and every value comes from a single fiscal year. It also
adds split-consistent prices, better XBRL tag fallbacks, readable messages for
SEC HTTP errors, and per-session caching of the SEC ticker file. See NEWS.md.

The package retrieves data from the SEC EDGAR XBRL API and Yahoo Finance. No
API key is required. All examples and vignette chunks are not evaluated
(\dontrun{} / eval = FALSE) because they need live internet access.
