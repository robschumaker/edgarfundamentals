.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "── \033[1mAttaching edgarfundamentals\033[0m ",
    paste(rep("─", 40), collapse=""),
    " edgarfundamentals ", utils::packageVersion("edgarfundamentals"), " ──\n",
    "\033[34mSet your User-Agent before making API calls:\033[0m\n",
    "  options(edgarfundamentals.user_agent = \"\033[34mYour Name your@email.com\033[0m\")"
  )
}