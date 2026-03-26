.onAttach <- function(libname, pkgname) {
  cli::cli_rule(
    left  = "{.strong Attaching edgarfundamentals}",
    right = paste0("edgarfundamentals ", utils::packageVersion("edgarfundamentals"))
  )
  cli::cli_text("{cli::col_red('Set your User-Agent before making API calls:')}")
  cli::cli_code(" options(edgarfundamentals.user_agent = \"Your Name your@email.com\")")
}