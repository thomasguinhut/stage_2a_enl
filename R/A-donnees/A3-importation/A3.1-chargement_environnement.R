
# -------------------------------------------------------------------
# 1. Configuration de l'environnement --------------------------------
.libPaths(c("/home/onyxia/work/userLibrary", .libPaths()))

options(
  renv.config.sandbox.enabled = FALSE,
  ask = FALSE,
  install.packages.check.source = "no"
)

# Définir une variable d'environnement GITHUB_PAT pour remotes::install_github
if (nzchar(Sys.getenv("GITHUB_PAT")) == FALSE) {
  Sys.setenv(GITHUB_PAT = "ghp_FAKE_TOKEN_FOR_NON_INTERACTIVE_USE_ONLY")
}

# -------------------------------------------------------------------
# 2. Détection versions et dépendances système -----------------------

message("Version R détectée : ", getRversion())

check_libgit2 <- tryCatch(
  suppressWarnings(system("ldconfig -p 2>/dev/null | grep libgit2", intern = TRUE)),
  error = function(e) character(0)
)

excluded <- c()

if (length(check_libgit2) == 0) {
  message("⚠️ libgit2 non détectée, exclusion de git2r")
  excluded <- c(excluded, "git2r")
} else {
  message("libgit2 détectée, git2r sera installé si besoin")
}

# -------------------------------------------------------------------
# 3. Chargement (ou installation) de renv ----------------------------

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", quiet = TRUE, ask = FALSE)
}

suppressMessages(suppressPackageStartupMessages({
  library(renv)
  options(
    renv.config.restore.prompt = FALSE,
    renv.consent = TRUE
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
  "data.table", "dplyr", "tidyr", "tidyverse", "readr",
  "stringr",
  "parallel", "foreach", "doParallel",
  "survey", "sampling",
  "ggplot2", "ggthemes", "ggh4x", "ggtext", "gridExtra", "grid",
  "rstudioapi", "aws.s3"
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
    if (!suppressMessages(require(pkg, character.only = TRUE, quietly = TRUE))) {
      message("⚠️ Impossible de charger le package : ", pkg)
    } else {
      message("✓ Package chargé : ", pkg)
    }
  }
}

invisible(lapply(deps, install_if_missing))

# -------------------------------------------------------------------
# 6. Installation optionnelle de rsthemes ----------------------------

try({
  if (!"rsthemes" %in% installed.packages()[, "Package"]) {
    if ("git2r" %in% excluded) {
      message("⚠️ Installation de rsthemes ignorée (dépendances système manquantes)")
    } else {
      if (!requireNamespace("remotes", quietly = TRUE)) {
        install.packages("remotes", quiet = TRUE, ask = FALSE)
      }
      withCallingHandlers(
        remotes::install_github("gadenbuie/rsthemes", quiet = TRUE),
        warning = function(w) invokeRestart("muffleWarning")
      )
    }
  }
}, silent = TRUE)

# -------------------------------------------------------------------
# 7. Snapshot renv ---------------------------------------------------

renv::snapshot(prompt = FALSE)
message("✅ Installation terminée et environnement enregistré avec renv.")

# -------------------------------------------------------------------
# 8. Nettoyage de l'environnement global -----------------------------

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

