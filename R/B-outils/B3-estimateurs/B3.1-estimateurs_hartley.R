estimateur_hartley <- function(donnees, scenario = "1", variantes, coef, estimateur) {
  for (variante in variantes) {
    donnees[[paste0("poids_", estimateur, "_", variante, "_", scenario)]] <-
      coef * donnees[[paste0("poids_", variante, "_", scenario)]]
  }
  
  return(donnees)
}
