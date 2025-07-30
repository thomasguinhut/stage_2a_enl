estimateurs_HT_classiques <- function(donnees, scenarios = c("1", "2"), variantes) {
  for (i in scenarios) {
    for (variante in variantes) {
      donnees[[paste0("poids_tirage_multi_", variante, "_", i)]] <-
        donnees[[paste0("poids_", variante, "_", i)]] * donnees[[paste0("rep_multi_", i)]]
      
      donnees[[paste0("poids_tirage_mono_", variante, "_", i)]] <-
        donnees[[paste0("poids_", variante, "_", i)]] * donnees[[paste0("rep_mono_", i)]]
    }
  }
  
  return(donnees)
}
