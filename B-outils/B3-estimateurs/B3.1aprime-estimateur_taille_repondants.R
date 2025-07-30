source("B-outils/B3-estimateurs/B3.1-estimateurs_hartley.R")

estimateur_1aprime <- function(donnees,
                               scenarios = c("1", "2"),
                               variantes) {
  
  for (i in scenarios) {
    
    # Nombre total de répondants dans le monomode
    nb_rep_mono <- sum(donnees[[paste0("rep_mono_", i)]], na.rm = TRUE)
    
    # Nombre total de répondants dans l'ensemble (mono + multi)
    nb_rep_total <- sum(donnees[[paste0("rep_", i)]], na.rm = TRUE)
    
    # Proportion de répondants issus du monomode
    prop_rep_mono <- nb_rep_mono / nb_rep_total
    
    # Coefficient selon l'appartenance à un sous-échantillon
    coef <- prop_rep_mono * donnees$ind_tirage_monomode +
      (1 - prop_rep_mono) * donnees$ind_tirage_multimode
    
    # Poids sans non-réponse pour ce scénario
    donnees$poids_1aprime_sans_nr <- coef * donnees$poids_tirage
    
    # Estimation Hartley
    donnees <- estimateur_hartley(donnees, i, variantes, coef * donnees[[paste0("rep_", i)]], "1aprime")
  }
  
  return(donnees)
}

