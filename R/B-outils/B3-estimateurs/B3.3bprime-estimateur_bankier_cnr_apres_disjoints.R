estimateur_3bprime <- function(donnees, scenarios, variantes) {
  
  denominateur <- donnees$proba_tirage_ech_multimode + donnees$proba_tirage_ech_monomode
  
  # Poids sans NR
  donnees$poids_3prime_sans_nr <- (1 / denominateur) * donnees$ind_tirage
  
  for (i in scenarios) {
    for (variante in variantes) {
      donnees[[paste0("poids_3bprime_", variante, "_", i)]] <- ifelse(
        donnees[[paste0("proba_", variante, "_", i)]] != 0,
        (1 / denominateur) * (1 / donnees[[paste0("proba_", variante, "_", i)]]) * donnees[[paste0("rep_", i)]],
        0
      )
    }
  }
  
  return(donnees)
}
