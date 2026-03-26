.onAttach <- function(libname, pkgname) {
  cli::cli_rule(
    left  = "{.strong Attaching edgarfundamentals}",
    right = paste0("edgarfundamentals ", utils::packageVersion("edgarfundamentals")),
    class = "packageStartupMessage"
  )
  cli::cli_text("{cli::col_blue('Set your User-Agent before making API calls:')}",
                class = "packageStartupMessage")
  cli::cli_text(paste0("\u00a0\u00a0options(edgarfundamentals.user_agent = ", cli::col_blue('"Your Name your@email.com"'), ")"),
                class = "packageStartupMessage")
  cli::cli_rule(class = "packageStartupMessage")
}