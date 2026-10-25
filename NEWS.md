# edgarfundamentals 0.1.3
    * Point-in-time results: `to_date` now selects the latest 10-K *filed* on or before it.
      Previously a fiscal year that ended before `to_date` was used even if its 10-K was
      filed later, and restated values from later filings leaked into past dates.
    * All values now come from a single fiscal year. Previously each concept took its own
      latest value, so a stale primary tag (e.g. NVDA, PFE revenue) or a concept the company
      stopped reporting (e.g. PFE, KO long-term debt) mixed figures from different years.
    * Flow concepts are restricted to fiscal-year-length periods, so fourth-quarter facts in
      a 10-K can no longer be taken as annual values.
    * Prices use the unadjusted close on or before `to_date` instead of the dividend-adjusted
      price, and per-share values are put on the same share basis when a stock split falls
      between the 10-K and `to_date` (affected PE, PB and DIV for any later split).
    * Shares outstanding falls back to the cover-page count (the dei fallback never worked).
    * Debt-to-equity uses total long-term debt including its current portion, with fallbacks
      for filers that tag only the noncurrent and current pieces or include finance leases.
    * Revenue prefers the total (Revenues) over contract revenue, which omits items such as
      alliance and royalty income.
    * Gross margin falls back to revenue minus cost of revenue when GrossProfit is not tagged.
    * 10-K/A and transition-period 10-KT filings are recognised.
    * SEC HTTP errors (403, 404, 429) now give a readable message; a warning is issued once
      if the User-Agent option is not set.
    * The SEC ticker file is cached per session (batches no longer download it per stock),
      and share-class tickers accept a dot or a dash (BRK.B or BRK-B).

# edgarfundamentals 0.1.2
    * Added .onAttach() startup message

# edgarfundamentals 0.1.1 (CRAN)
    * Added PB, DIV, ROA, GrossMargin, OperatingMargin, NetMargin, CurrentRatio, Revenue

# edgarfundamentals 0.1.0
    * New library that scrapes SEC 'EDGAR' filings for CIK, EPS, NetIncome, ROE, DE, PE
    * get_cik(), get_fundamentals(), get_fundamentals_batch(), get_filing_history()
    
