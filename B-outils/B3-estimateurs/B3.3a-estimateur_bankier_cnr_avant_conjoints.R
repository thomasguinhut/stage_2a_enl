estimateur_3a <- function(donnees, scenarios, variantes) {
  
  denominateur <- (donnees$proba_tirage_ech_multimode + donnees$proba_tirage_ech_monomode -
                     donnees$proba_tirage_ech_multimode * donnees$proba_tirage_ech_monomode)
  
  donnees$poids_3_sans_nr <- (1 / denominateur) * donnees$ind_tirage
  
  for (i in scenarios) {
    
    proba_multi_sans_grh <-  donnees[[paste0("proba_rep_multi_", i)]] * donnees$proba_tirage_ech_multimode
    proba_mono_sans_grh  <- donnees[[paste0("proba_rep_mono_", i)]] * donnees$proba_tirage_ech_monomode
    
    proba_multi_cnr_exacte <-  (donnees[[paste0("pi_int_", i)]] + (1 - donnees[[paste0("pi_int_", i)]]) * donnees[[paste0("pi_tel_non_int_", i)]]) * donnees$proba_tirage_ech_multimode
    proba_mono_cnr_exacte  <- donnees[[paste0("pi_int_", i)]] * donnees$proba_tirage_ech_monomode
  
    donnees[[paste0("poids_3a_cnr_exacte_", i)]] <- 1 / (proba_multi_cnr_exacte  + proba_mono_cnr_exacte - proba_multi_cnr_exacte * proba_mono_cnr_exacte)
    
    donnees[[paste0("poids_3a_sans_grh_", i)]] <- 1 / (
      proba_multi_sans_grh + proba_mono_sans_grh - 
        proba_multi_sans_grh * proba_mono_sans_grh)
    
  }
  
  return(donnees)
}
