estimateur_elliott <- function(donnees, scenarios, variantes, type = c("total", "A", "B")) {
  type <- match.arg(type)
  
  # Variables explicatives
  variables_x <- c("x_1", "x_2", "x_3", "x_4", "x_5")
  formule_x <- paste(variables_x, collapse = " + ")
  
  # Données des individus tirés
  donnees_modele <- donnees[donnees$ind_tirage == 1, ]
  
  # Modèles logistiques
  formule_multi <- as.formula(paste0("ind_tirage_multimode ~ ", formule_x))
  modele_multi <- glm(formule_multi, data = donnees_modele, family = binomial())
  prob_multi_rep_cond <- predict(modele_multi, newdata = donnees, type = "response") * (donnees$ind_tirage == 1)
  
  formule_mono <- as.formula(paste0("ind_tirage_monomode ~ ", formule_x))
  modele_mono <- glm(formule_mono, data = donnees_modele, family = binomial())
  prob_mono_rep_cond <- predict(modele_mono, newdata = donnees, type = "response") * (donnees$ind_tirage == 1)
  
  # Calcul de theta_k
  numerateur   <- prob_multi_rep_cond / donnees$proba_tirage_ech_multimode
  denominateur <- prob_mono_rep_cond / donnees$proba_tirage_ech_monomode
  theta_k <- ifelse(denominateur > 0, numerateur / denominateur, 0)
  
  # Poids de base
  poids_multi <- donnees$poids_tirage_multimode
  poids_mono  <- donnees$poids_tirage_monomode
  
  # Espérances conditionnelles
  if (type == "total") {
    mask_mono_rep <- donnees$ind_tirage == 1
    esperance <- sum(theta_k[mask_mono_rep] * poids_mono[mask_mono_rep]) / 
      sum(poids_mono[mask_mono_rep])
    donnees$poids_4_sans_nr <- 1/2 * ((theta_k / esperance) * poids_mono + poids_multi)
    
  } else if (type == "A") {
    mask_mono_rep_A <- donnees$ind_tirage == 1 & donnees$strate_vec == "A"
    esperance_A <- sum(theta_k[mask_mono_rep_A] * poids_mono[mask_mono_rep_A]) / 
      sum(poids_mono[mask_mono_rep_A])
    donnees$poids_4A_sans_nr <- 1/2 * ((theta_k / esperance_A) * poids_mono + poids_multi)
    
  } else if (type == "B") {
    mask_mono_rep_B <- donnees$ind_tirage == 1 & donnees$strate_vec == "B"
    esperance_B <- sum(theta_k[mask_mono_rep_B] * poids_mono[mask_mono_rep_B]) / 
      sum(poids_mono[mask_mono_rep_B])
    donnees$poids_4B_sans_nr <- 1/2 * ((theta_k / esperance_B) * poids_mono + poids_multi)
  }
  
  
  for (i in scenarios){
    
    for (variante in variantes){
      
      # Données des individus tirés
      donnees_modele <- donnees[donnees[[paste0("rep_", i)]] == 1, ]
      
      # Modèles logistiques
      formule_multi <- as.formula(paste0("rep_multi_", i, " ~ ", formule_x))
      modele_multi <- glm(formule_multi, data = donnees_modele, family = binomial())
      prob_multi_rep_cond <- predict(modele_multi, newdata = donnees, type = "response") * (donnees[[paste0("rep_", i)]] == 1)
      
      formule_mono <- as.formula(paste0("rep_mono_", i, " ~ ", formule_x))
      modele_mono <- glm(formule_mono, data = donnees_modele, family = binomial())
      prob_mono_rep_cond <- predict(modele_mono, newdata = donnees, type = "response") * (donnees[[paste0("rep_", i)]] == 1)
      
      # Calcul de theta_k
      numerateur   <- prob_multi_rep_cond / donnees$proba_tirage_ech_multimode
      denominateur <- prob_mono_rep_cond / donnees$proba_tirage_ech_monomode
      theta_k <- ifelse(denominateur > 0, numerateur / denominateur, 0)
      
      # Poids de base
      poids_multi <- donnees[[paste0("poids_", variante, "_", i)]] * donnees$ind_tirage_multimode
      poids_mono  <- donnees[[paste0("poids_", variante, "_", i)]] * donnees$ind_tirage_monomode
      
      # Espérances conditionnelles
      if (type == "total") {
        mask_mono_rep <- donnees[[paste0("rep_", i)]] == 1
        esperance <- sum(theta_k[mask_mono_rep] * poids_mono[mask_mono_rep]) / 
          sum(poids_mono[mask_mono_rep])
        donnees[[paste0("poids_4_", variante, "_", i)]] <- 1/2 * ((theta_k / esperance) * poids_mono + poids_multi)
        
      } else if (type == "A") {
        mask_mono_rep_A <- donnees[[paste0("rep_", i)]] == 1 & donnees$strate_vec == "A"
        esperance_A <- sum(theta_k[mask_mono_rep_A] * poids_mono[mask_mono_rep_A]) / 
          sum(poids_mono[mask_mono_rep_A])
        donnees[[paste0("poids_4A_", variante, "_", i)]] <- 1/2 * ((theta_k / esperance_A) * poids_mono + poids_multi)
        
      } else if (type == "B") {
        mask_mono_rep_B <- donnees[[paste0("rep_", i)]] == 1 & donnees$strate_vec == "B"
        esperance_B <- sum(theta_k[mask_mono_rep_B] * poids_mono[mask_mono_rep_B]) / 
          sum(poids_mono[mask_mono_rep_B])
        donnees[[paste0("poids_4B_", variante, "_", i)]] <- 1/2 * ((theta_k / esperance_B) * poids_mono + poids_multi)
      }
      
    }
    
  }
  
  
  return(donnees)
}