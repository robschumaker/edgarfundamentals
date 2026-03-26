.onAttach <- function(libname, pkgname) {
  ver   <- as.character(utils::packageVersion("edgarfundamentals"))
  packageStartupMessage(
    cli::rule(left = paste("edgarfundamentals", ver)), "\n",
    cli::col_blue("Set your User-Agent before making API calls:"), "\n",
    "  options(edgarfundamentals.user_agent = \"",
    cli::col_blue("Your Name your@email.com"), "\")\n",
    cli::rule()
  )
}