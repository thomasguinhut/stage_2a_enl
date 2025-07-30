source("B-outils/B3-estimateurs/B3.1-estimateurs_hartley.R")

estimateur_2a <- function(donnees, scenarios = c("1", "2"), variantes) {
  
  # Créer une copie pour éviter les problèmes de référence
  resultat <- copy(donnees)
  
  # Initialiser la colonne des poids
  resultat$poids_2a_sans_nr <- NA_real_
  
  # Traiter chaque strate séparément
  for (strate in unique(donnees$strate_vec)) {
    
    # Sous-échantillon de la strate
    ind_strate <- donnees$strate_vec == strate
    donnees_strate <- donnees[ind_strate]
    
    # Calculs pour cette strate
    taille_mono <- sum(donnees_strate$ind_tirage_monomode, na.rm = TRUE)
    taille_multi <- sum(donnees_strate$ind_tirage_multimode, na.rm = TRUE)
    total <- taille_mono + taille_multi
    prop_mono <- taille_mono / total
    
    # Coefficient pour cette strate
    coef <- prop_mono * donnees_strate$ind_tirage_monomode + 
      (1 - prop_mono) * donnees_strate$ind_tirage_multimode
    
    # Poids sans NR
    resultat$poids_2a_sans_nr[ind_strate] <- coef * donnees_strate$poids_tirage
    
    # Appliquer Hartley pour chaque scénario
    for (i in scenarios) {
      donnees_strate <- estimateur_hartley(donnees_strate, i, variantes, 
                                           coef * donnees_strate[[paste0("rep_", i)]], "2a")
    }
    
    # Récupérer les colonnes créées par Hartley
    nouvelles_cols <- setdiff(names(donnees_strate), names(donnees))
    for (col in nouvelles_cols) {
      if (!col %in% names(resultat)) {
        resultat[[col]] <- NA_real_
      }
      resultat[[col]][ind_strate] <- donnees_strate[[col]]
    }
  }
  
  return(resultat)
}