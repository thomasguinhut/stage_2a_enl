export_resultats <- function(resultats, chiffre, chemin_dossier, nom_dossier){

  saveRDS(resultats$brut, paste0(chemin_dossier, "/D", chiffre, ".1-brut_", nom_dossier, ".rds"))
  saveRDS(resultats$biais, paste0(chemin_dossier, "/D", chiffre, ".2-biais_", nom_dossier, ".rds"))
  saveRDS(resultats$taux_rep_grh, paste0(chemin_dossier, "/D", chiffre, ".3-taux_rep_grh_", nom_dossier, ".rds"))
}
