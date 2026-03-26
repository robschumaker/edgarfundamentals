.onAttach <- function(libname, pkgname) {
  cli::cli_alert_info("edgarfundamentals v{utils::packageVersion('edgarfundamentals')}")
  cli::cli_alert_warning("Please set your User-Agent before making API calls:")
  cli::cli_code("options(edgarfundamentals.user_agent = \"Your Name your@email.com\")")
}