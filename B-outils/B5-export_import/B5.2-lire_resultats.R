lire_resultats <- function(num_dossier, nom_dossier, chemin_dossier, scenarios, nom_methodes, prefix_var_interet, nb_sim) {
  
  # Lecture
  brut <- readRDS(paste0(chemin_dossier, "/D", num_dossier, ".1-brut_", nom_dossier, ".rds"))
  biais <- readRDS(paste0(chemin_dossier, "/D", num_dossier, ".2-biais_", nom_dossier, ".rds"))
  taux_rep_grh <- readRDS(paste0(chemin_dossier, "/D", num_dossier, ".3-taux_rep_grh_", nom_dossier, ".rds"))
  
  # Assignation
  assign(paste0(nom_dossier, "_brut"), brut, envir = .GlobalEnv)
  assign(paste0(nom_dossier, "_biais"), biais, envir = .GlobalEnv)
  assign(paste0(nom_dossier, "_taux_rep_grh"), taux_rep_grh, envir = .GlobalEnv)
  
  # Affichage lisible
  cat("\n", strrep("-", 50), "\n")
  cat("\u2713  Objet : brut_", nom_dossier, "\n", sep = "")
  str(brut, max.level = 2, vec.len = 5)
  
  cat("\n", strrep("-", 50), "\n")
  cat("\u2713  Objet : biais_", nom_dossier, "\n", sep = "")
  str(biais, max.level = 2, vec.len = 5)
  cat(strrep("-", 50), "\n")
  
  nbr_lignes <- (
    length(nom_methodes) + 2 - ifelse(any(c("3a", "3aprime") %in% nom_methodes), 1, 0) - ifelse(any(c("3b", "3bprime") %in% nom_methodes), 1, 0) +
    length(nom_methodes) + 2 +
    length(nom_methodes) + 2 +
    length(nom_methodes) + 2 - ifelse(any(c("3a", "3aprime") %in% nom_methodes), 2, 0)
                 ) * length(prefix_var_interet) * 3
  
  cat(paste0("\nLe fichier ", nom_dossier, "_brut doit contenir ",
             (nbr_lignes + 3 * length(prefix_var_interet)) * nb_sim,
             " lignes.\n"))
  
  cat(paste0("Le fichier ", nom_dossier, "_biais doit contenir ",
             nbr_lignes * nb_sim,
             " lignes.\n"))
}
