suppressPackageStartupMessages(suppressMessages(rsthemes::install_rsthemes()))

if (rstudioapi::isAvailable()) {
  tryCatch(
    {
      # On retire {rsthemes} car ce n'est pas un nom valide
      rstudioapi::applyTheme("a11y-dark")
      message("Thème 'a11y-dark' appliqué avec succès.")
    },
    error = function(e) {
      # On ignore l'erreur et on ne l'affiche pas
      invisible(NULL)
    }
  )
} else {
  warning("Le thème ne peut être appliqué que dans une session RStudio.")
}
