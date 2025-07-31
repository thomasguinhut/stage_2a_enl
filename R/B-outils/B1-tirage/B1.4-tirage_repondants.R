source("B-outils/B1-tirage/B1.2-intersection_depuis_fin.R")
source("B-outils/B1-tirage/B1.3-reponse_via_utilite.R")

tirage_repondants <- function(donnees,
                              sigma = NULL,
                              latent = TRUE,
                              scenario,
                              nom_var_prob_internet = "pi_int",
                              nom_var_prob_tel = "pi_tel",
                              suffixe = NULL) {
  
  if (latent) {
    # === MODE LATENT ===
    
    # Définir les variables d’utilité
    nom_var_utilite_internet <- paste0("utilite_int_", scenario)
    nom_var_utilite_tel <- paste0("utilite_tel_", scenario)
    part_nom_int <- gsub("^utilite_", "", nom_var_utilite_internet)
    part_nom_tel <- gsub("^utilite_", "", nom_var_utilite_tel)
    
    # Noms des colonnes réponse simulée
    nom_var_rep_internet_multi <- paste0("rep_multi_", part_nom_int)
    nom_var_rep_internet_mono <- paste0("rep_mono_", part_nom_int)
    
    # Couples d’utilités
    couples_utilite <- mapply(function(x, y) c(x, y),
                              sort(nom_var_utilite_internet),
                              sort(nom_var_utilite_tel),
                              SIMPLIFY = FALSE)
    
    # Création de suffixes pour les noms
    suffixe_pour_nouvelles_var <- lapply(couples_utilite, function(x) {
      intersection_depuis_fin(x[1], x[2])
    })
    
    # Simulation multimode
    reponse_multimode <- lapply(couples_utilite, function(nom_var) {
      reponse_multimode_via_utilite(donnees = donnees, nom_colonne_utilite = nom_var, sigma = sigma)
    })
    reponse_multimode_totale <- Reduce(cbind, lapply(reponse_multimode, rowSums))
    if (!is.matrix(reponse_multimode_totale)) {
      reponse_multimode_totale <- matrix(reponse_multimode_totale, ncol = 1)
    }
    colnames(reponse_multimode_totale) <- paste0("rep_multi", suffixe_pour_nouvelles_var)
    reponse_multimode <- Reduce(cbind, reponse_multimode)
    nom_col_multimode <- paste0("rep_multi_", gsub("^utilite_", "", unlist(couples_utilite)))
    colnames(reponse_multimode) <- nom_col_multimode
    reponse_multimode <- cbind(reponse_multimode, reponse_multimode_totale)
    
    # Simulation monomode
    reponse_monomode <- lapply(couples_utilite, function(nom_var) {
      reponse_monomode_via_utilite(donnees = donnees, nom_colonne_utilite = nom_var, sigma = sigma)
    })
    reponse_monomode_totale <- Reduce(cbind, lapply(reponse_monomode, rowSums))
    if (!is.matrix(reponse_monomode_totale)) {
      reponse_monomode_totale <- matrix(reponse_monomode_totale, ncol = 1)
    }
    colnames(reponse_monomode_totale) <- paste0("rep_mono", suffixe_pour_nouvelles_var)
    reponse_monomode <- Reduce(cbind, reponse_monomode)
    nom_col_monomode <- paste0("rep_mono_", gsub("^utilite_", "", unlist(couples_utilite)))
    colnames(reponse_monomode) <- nom_col_monomode
    reponse_monomode <- cbind(reponse_monomode, reponse_monomode_totale)
    
    # Mise à zéro des non-tirés
    reponse_monomode[] <- reponse_monomode * donnees$ind_tirage_monomode
    reponse_multimode[] <- reponse_multimode * donnees$ind_tirage_multimode
    
    # Fusion
    reponse <- cbind(reponse_monomode, reponse_multimode)
    
    # Ajout de la réponse totale par scénario
    reponse_globale <- reponse[, paste0("rep_multi", suffixe_pour_nouvelles_var)] +
      reponse[, paste0("rep_mono", suffixe_pour_nouvelles_var)]
    if (!is.matrix(reponse_globale)) {
      reponse_globale <- matrix(reponse_globale, ncol = 1)
    }
    colnames(reponse_globale) <- paste0("rep", suffixe_pour_nouvelles_var)
    
    donnees <- cbind(donnees, reponse, reponse_globale)
    
  } else {
    # === MODE NON-LATENT ===
    
    n <- nrow(donnees)
    
    partie_nom_int <- gsub("^pi_", "", nom_var_prob_internet)
    partie_nom_tel <- gsub("^pi_", "", nom_var_prob_tel)
    
    nom_rep_int_multi <- paste0("rep_", partie_nom_int, "_multi", suffixe)
    nom_rep_int_mono  <- paste0("rep_", partie_nom_int, "_mono", suffixe)
    nom_rep_tel_multi <- paste0("rep_", partie_nom_tel, "_multi", suffixe)
    nom_rep_multi     <- paste0("rep_multi", suffixe)
    nom_rep_total     <- paste0("rep", suffixe)
    
    # Simulation internet
    donnees[[nom_rep_int_multi]] <- runif(n) < donnees[[nom_var_prob_internet]]
    donnees[[nom_rep_int_mono]]  <- donnees[[nom_rep_int_multi]]
    
    # Mise à zéro pour les non tirés
    donnees[[nom_rep_int_multi]] <- donnees[[nom_rep_int_multi]] * donnees$ind_tirage_multimode
    donnees[[nom_rep_int_mono]]  <- donnees[[nom_rep_int_mono]] * donnees$ind_tirage_monomode
    
    # Simulation téléphone dans l’échantillon multimode uniquement
    donnees[[nom_rep_tel_multi]] <- as.integer(runif(n) < donnees[[nom_var_prob_tel]])
    donnees[[nom_rep_tel_multi]] <- donnees[[nom_rep_tel_multi]] * 
      (1 - donnees[[nom_rep_int_multi]]) *
      donnees$ind_tirage_multimode
    
    # Réponses multimode (internet ou téléphone)
    donnees[[nom_rep_multi]] <- donnees[[nom_rep_int_multi]] + donnees[[nom_rep_tel_multi]]
    
    # Réponse totale tous modes
    donnees[[nom_rep_total]] <- donnees[[nom_rep_multi]] + donnees[[nom_rep_int_mono]]
  }
  
  return(donnees)
}
