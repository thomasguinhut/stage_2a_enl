# setup.R
# Script non interactif pour installer les packages nécessaires
# dans un environnement RStudio Onyxia, avec gestion renv

# -------------------------------------------------------------------
# 1. Configuration de l'environnement --------------------------------
.libPaths(c("/home/onyxia/work/userLibrary", .libPaths()))

# Options pour désactiver toutes les interactions utilisateur
options(
  renv.config.sandbox.enabled = FALSE,
  ask = FALSE,
  install.packages.check.source = "no"
)

# -------------------------------------------------------------------
# 3. Chargement (ou installation) de renv ----------------------------

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", quiet = TRUE, ask = FALSE)
}

suppressMessages(suppressPackageStartupMessages({
  library(renv)
  options(
    renv.config.restore.prompt = FALSE,
    renv.consent = TRUE             # ✅ consentement explicite
  )
  Sys.setenv(RENV_FORCE_PROMPT = "FALSE")
  
  if (!file.exists("renv.lock")) {
    message("🔧 Aucun renv.lock détecté, initialisation de renv...")
    renv::init(bare = TRUE, restart = FALSE, prompt = FALSE)
  } else {
    renv::restore(prompt = FALSE)
  }
}))

# -------------------------------------------------------------------
# 4. Liste des packages à installer et charger -----------------------

deps <- c(
  # Pour gestion data et manipulation
  "data.table", "dplyr", "tidyr", "tidyverse", "readr",
  
  # Pour string manipulation
  "stringr",
  
  # Parallélisme et calcul
  "parallel", "foreach", "doParallel",
  
  # Pour sondage, tirage et estimations
  "survey", "sampling",
  
  # Visualisation et graphiques
  "ggplot2", "ggthemes", "ggh4x", "ggtext", "gridExtra", "grid",
  
  # Outils spécifiques
  "aws.s3"
)

# -------------------------------------------------------------------
# 5. Installation des packages manquants -----------------------------

# Fournir de fausses clés pour éviter le warning aws.s3
if (nzchar(Sys.getenv("AWS_ACCESS_KEY_ID")) == FALSE) {
  Sys.setenv("AWS_ACCESS_KEY_ID" = "dummy", "AWS_SECRET_ACCESS_KEY" = "dummy")
}

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("→ Installation du package : ", pkg)
    install.packages(pkg, quiet = TRUE, ask = FALSE)
  }
  
  # Charger le package avec traitement spécial pour aws.s3
  if (pkg == "aws.s3") {
    suppressWarnings({
      if (!nzchar(Sys.getenv("AWS_ACCESS_KEY_ID"))) {
        Sys.setenv("AWS_ACCESS_KEY_ID" = "dummy", "AWS_SECRET_ACCESS_KEY" = "dummy")
      }
      if (!suppressMessages(require(pkg, character.only = TRUE, quietly = TRUE))) {
        message("⚠️ Impossible de charger le package : ", pkg)
      } else {
        message("✓ Package chargé : ", pkg, " (credentials AWS fictifs utilisés)")
      }
    })
  } else {
    # Charger tous les autres packages normalement
    if (!suppressMessages(require(pkg, character.only = TRUE, quietly = TRUE))) {
      message("⚠️ Impossible de charger le package : ", pkg)
    } else {
      message("✓ Package chargé : ", pkg)
    }
  }
}

invisible(lapply(deps, install_if_missing))

# -------------------------------------------------------------------
# 6. Snapshot renv ---------------------------------------------------

renv::snapshot(prompt = FALSE)
message("✅ Installation terminée et environnement enregistré avec renv.")

# -------------------------------------------------------------------
# 7. Nettoyage de l'environnement global -----------------------------

exceptions <- c(
  "bdd", "sigma_1", "resultats",
  ls(pattern = "biais$"),
  ls(pattern = "brut$"),
  ls(pattern = "taux_rep_grh$"),
  ls(pattern = "poids_moyens$")
)

exceptions_df <- exceptions[sapply(exceptions, function(obj) {
  exists(obj, envir = .GlobalEnv) &&
    (
      is.data.frame(get(obj, envir = .GlobalEnv)) ||
        (requireNamespace("data.table", quietly = TRUE) && data.table::is.data.table(get(obj, envir = .GlobalEnv))) ||
        is.matrix(get(obj, envir = .GlobalEnv))
    )
})]

rm(list = setdiff(ls(envir = .GlobalEnv), exceptions_df), envir = .GlobalEnv)
