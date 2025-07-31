source("B-outils/B3-estimateurs/B3.1-estimateurs_hartley.R")

estimateur_1a <- function(donnees,
                          scenarios = c("1", "2"),
                          variantes) {
  
  # Taille des sous-échantillons
  taille_mono <- sum(donnees$ind_tirage_monomode, na.rm = TRUE)
  taille_multi <- sum(donnees$ind_tirage_multimode, na.rm = TRUE)
  
  # Proportion d’individus dans le monomode
  prop_mono <- taille_mono / (taille_mono + taille_multi)
  
  # Coefficient d’appartenance à chaque strate selon la méthode 1a
  coef <- prop_mono * donnees$ind_tirage_monomode +
    (1 - prop_mono) * donnees$ind_tirage_multimode
  
  # Calcul des poids avant la boucle
  donnees$poids_1a_sans_nr <- coef * donnees$poids_tirage
  
  # Boucle sur les scénarios
  for (i in scenarios) {
    donnees <- estimateur_hartley(donnees, i, variantes, coef * donnees[[paste0("rep_", i)]], "1a")
  }
  
  return(donnees)
}