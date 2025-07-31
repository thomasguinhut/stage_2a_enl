# setup.R
# Script pour installer les packages nécessaires au projet
# dans un environnement RStudio Onyxia, avec gestion renv

# -------------------------------------------------------------------
# 1. Configuration de l'environnement --------------------------------
.libPaths(c("/home/onyxia/work/userLibrary", .libPaths()))
options(renv.config.sandbox.enabled = FALSE)

# -------------------------------------------------------------------
# 2. Chargement (ou installation) de renv ----------------------------
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}
library(renv)

# -------------------------------------------------------------------
# 3. Détection des dépendances ---------------------------------------
deps <- unique(renv::dependencies(path = ".", progress = FALSE)$Package)

# Exclusion des packages problématiques ou non nécessaires
excluded <- c("renv", "git2r", "rsthemes")
deps <- setdiff(deps, excluded)

# -------------------------------------------------------------------
# 4. Installation des packages manquants -----------------------------
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("→ Installation du package : ", pkg)
    install.packages(pkg)
  } else {
    message("✓ Package déjà installé : ", pkg)
  }
}
invisible(lapply(deps, install_if_missing))

# -------------------------------------------------------------------
# 5. Installation optionnelle de rsthemes ----------------------------
# Attention : peut échouer dans Onyxia si dépendances système manquantes
try({
  if (!requireNamespace("rsthemes", quietly = TRUE)) {
    if (!requireNamespace("devtools", quietly = TRUE)) {
      install.packages("devtools")
    }
    devtools::install_github("gadenbuie/rsthemes")
  }
}, silent = TRUE)

# -------------------------------------------------------------------
# 6. Snapshot renv ---------------------------------------------------
renv::snapshot(prompt = FALSE)
message("✅ Installation terminée et environnement enregistré avec renv.")

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
        data.table::is.data.table(get(obj, envir = .GlobalEnv)) || 
        is.matrix(get(obj, envir = .GlobalEnv))
    )
})]

rm(list = setdiff(ls(envir = .GlobalEnv), exceptions_df), envir = .GlobalEnv)

