source("B-outils/B2-correction_non_reponse/B2.2-cnr_mode.R")

cnr_finale <- function(donnees,
                       formule_cnr,
                       scenarios,
                       grh,
                       taux_min_grh) {
  
  for (i in scenarios){
    
    nom_var_rep_total <- paste0("rep_", i)
    
    # On crée un identifiant temporaire pour garder l'ordre initial
    donnees$id_temp <- 1:nrow(donnees)
    
    # Séparation des sous-échantillons
    donnees_multi <- donnees[donnees$ind_tirage_multimode == 1, , drop = FALSE]
    donnees_mono  <- donnees[donnees$ind_tirage_monomode == 1, , drop = FALSE]
    donnees_hors_ech <- donnees[
      donnees$ind_tirage_multimode != 1 & donnees$ind_tirage_monomode != 1, , drop = FALSE]
    
    # Si hors échantillon vide, on crée un data.frame vide mais avec mêmes colonnes
    if (nrow(donnees_hors_ech) == 0) {
      donnees_hors_ech <- donnees[FALSE, ]
    }
    
    formule_cnr_i <- as.formula(paste0("rep_", i, " ~ ", formule_cnr))
    
    # Application des corrections sur les deux sous-échantillons ET récupération des modèles
    resultat_multi <- cnr_mode(donnees_multi, formule_cnr_i, i, "multi", grh, taux_min_grh)
    resultat_mono  <- cnr_mode(donnees_mono, formule_cnr_i, i, "mono", grh, taux_min_grh)
    
    donnees_multi <- resultat_multi$donnees
    donnees_mono  <- resultat_mono$donnees
    modele_multi  <- resultat_multi$modele
    modele_mono   <- resultat_mono$modele
    
    # NOUVEAU : Calcul des probas de réponse pour TOUS les individus
    # Proba multimode pour tous
    if (!is.null(modele_multi) && nrow(donnees_multi) > 0) {
      donnees[[paste0("proba_rep_multi_", i)]] <- as.numeric(predict(modele_multi, newdata = donnees, type = "response"))
    } else {
      donnees[[paste0("proba_rep_multi_", i)]] <- NA
    }
    
    # Proba monomode pour tous
    if (!is.null(modele_mono) && nrow(donnees_mono) > 0) {
      donnees[[paste0("proba_rep_mono_", i)]] <- as.numeric(predict(modele_mono, newdata = donnees, type = "response"))
    } else {
      donnees[[paste0("proba_rep_mono_", i)]] <- NA
    }
    
    # Pour les individus hors échantillon, poids corrigés = 0
    donnees_hors_ech[[paste0("poids_cnr_exacte_", i)]] <- 0
    donnees_hors_ech[[paste0("proba_cnr_exacte_", i)]] <- 0
    donnees_hors_ech[[paste0("poids_cnr_sans_grh_", i)]] <- 0
    donnees_hors_ech[[paste0("proba_cnr_sans_grh_", i)]] <- 0
    if (!is.null(grh)) {
      donnees_hors_ech[[paste0("poids_cnr_avec_grh_", i)]] <- 0
      donnees_hors_ech[[paste0("proba_cnr_avec_grh_", i)]] <- 0
      donnees_hors_ech[[paste0("grh_groupe_", i)]] <- NA
    }
    
    # Vérification et homogénéisation des classes des colonnes pour éviter erreur rbind
    all_data <- list(donnees_multi, donnees_mono, donnees_hors_ech)
    
    # Trouver toutes les colonnes
    all_cols <- unique(unlist(lapply(all_data, names)))
    
    # Convertir colonnes facteurs en caractères dans tous les tableaux
    for (col in all_cols) {
      for (j in seq_along(all_data)) {
        if (col %in% names(all_data[[j]])) {
          if (is.factor(all_data[[j]][[col]])) {
            all_data[[j]][[col]] <- as.character(all_data[[j]][[col]])
          }
        }
      }
    }
    
    donnees_multi <- all_data[[1]]
    donnees_mono  <- all_data[[2]]
    donnees_hors_ech <- all_data[[3]]
    
    # Fusionner les données corrigées
    donnees_fusionnees <- rbind(donnees_multi, donnees_mono, donnees_hors_ech)
    
    # Remettre l'ordre initial
    donnees_fusionnees <- donnees_fusionnees[order(donnees_fusionnees$id_temp), ]
    
    # Supprimer la colonne temporaire
    donnees_fusionnees$id_temp <- NULL
    
    # Ajouter les nouvelles variables de proba à la base fusionnée
    donnees_fusionnees[[paste0("proba_rep_multi_", i)]] <- donnees[[paste0("proba_rep_multi_", i)]]
    donnees_fusionnees[[paste0("proba_rep_mono_", i)]] <- donnees[[paste0("proba_rep_mono_", i)]]
    
    donnees <- donnees_fusionnees
    
  }
  
  # Retourner le résultat final
  return(donnees)
}