chargement_theme_rstudio <- function(theme_name = "a11y-dark {rsthemes}") {
  # Installer pacman si nécessaire
  if (!requireNamespace("pacman", quietly = TRUE)) {
    install.packages("pacman")
  }
  
  # Charger les packages nécessaires
  pacman::p_load(devtools, rstudioapi, character.only = FALSE)
  
  # Installer rsthemes si absent
  if (!"rsthemes" %in% installed.packages()[, "Package"]) {
    devtools::install_github("gadenbuie/rsthemes", upgrade = "never")
  }
  
  # Charger rsthemes
  pacman::p_load(rsthemes)
  
  # Installer les thèmes (idempotent, sans danger)
  rsthemes::install_rsthemes()
  
  # Appliquer le thème si RStudio est actif
  if (rstudioapi::isAvailable()) {
    rstudioapi::applyTheme(theme_name)
    message(sprintf("Thème '%s' appliqué avec succès.", theme_name))
  } else {
    warning("Le thème ne peut être appliqué que dans une session RStudio.")
  }
}
