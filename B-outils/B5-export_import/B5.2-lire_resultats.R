lire_resultats <- function(num_dossier, nom_dossier, chemin_dossier, scenarios, nom_methodes, prefix_var_interet, nb_sim, aws) {
  
  # Lecture
  if (aws == FALSE) {
    
    brut <- readRDS(paste0(chemin_dossier, "/D", num_dossier, ".1-brut_", nom_dossier, ".rds"))
    biais <- readRDS(paste0(chemin_dossier, "/D", num_dossier, ".2-biais_", nom_dossier, ".rds"))
    taux_rep_grh <- readRDS(paste0(chemin_dossier, "/D", num_dossier, ".3-taux_rep_grh_", nom_dossier, ".rds"))
    
  } else {
    
    brut <- aws.s3::s3read_using(
      FUN = function(file) read_csv(file, show_col_types = FALSE),
      object = paste0(chemin_dossier, "/", num_dossier, ".1-brut_", nom_dossier, ".csv"),
      bucket = "thomasguinhut",
      opts = list("region" = "")
    )
    
    biais <- aws.s3::s3read_using(
      FUN = function(file) read_csv(file, show_col_types = FALSE),
      object = paste0(chemin_dossier, "/", num_dossier, ".2-biais_", nom_dossier, ".csv"),
      bucket = "thomasguinhut",
      opts = list("region" = "")
    )
    
    taux_rep_grh <- aws.s3::s3read_using(
      FUN = function(file) read_csv(file, show_col_types = FALSE),
      object = paste0(chemin_dossier, "/", num_dossier, ".3-taux_rep_grh_", nom_dossier, ".csv"),
      bucket = "thomasguinhut",
      opts = list("region" = "")
    )
    
  }
  
  # Conversion en data.frame pour assignation et affichage
  brut_df <- as.data.frame(brut)
  biais_df <- as.data.frame(biais)
  taux_rep_grh_df <- as.data.frame(taux_rep_grh)
  
  # Assignation dans l’environnement global
  assign(paste0(nom_dossier, "_brut"), brut_df, envir = .GlobalEnv)
  assign(paste0(nom_dossier, "_biais"), biais_df, envir = .GlobalEnv)
  assign(paste0(nom_dossier, "_taux_rep_grh"), taux_rep_grh_df, envir = .GlobalEnv)
  
  # Affichage lisible sur les versions data.frame
  cat("\n", strrep("-", 50), "\n")
  cat("\u2713  Objet : brut_", nom_dossier, "\n", sep = "")
  str(brut_df, max.level = 2, vec.len = 5)
  
  cat("\n", strrep("-", 50), "\n")
  cat("\u2713  Objet : biais_", nom_dossier, "\n", sep = "")
  str(biais_df, max.level = 2, vec.len = 5)
  
  cat(strrep("-", 50), "\n")
  
  nbr_lignes <- (
    length(nom_methodes) + 2 - ifelse(any(c("3a", "3aprime") %in% nom_methodes), 1, 0) - ifelse(any(c("3b", "3bprime") %in% nom_methodes), 1, 0) +
      (length(nom_methodes) + 2) * length(scenarios) +
      (length(nom_methodes) + 2) * length(scenarios) +
      (length(nom_methodes) + 2 - ifelse(any(c("3a", "3aprime") %in% nom_methodes), 2, 0)) * length(scenarios)
  ) * length(prefix_var_interet) * 3
  
  cat(paste0("\nLe fichier ", nom_dossier, "_brut doit contenir ",
             (nbr_lignes * nb_sim + 3 * length(prefix_var_interet)),
             " lignes.\n"))
  
  cat(paste0("Le fichier ", nom_dossier, "_biais doit contenir ",
             nbr_lignes * nb_sim,
             " lignes.\n"))
}
