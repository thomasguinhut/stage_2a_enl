source("B-outils/B2-correction_non_reponse/B2.1-ghr_quantile.R")

cnr_mode <- function(donnees,
                     formule_cnr,
                     scenario,
                     mode = c("mono", "multi"),
                     grh,
                     taux_min_grh) {
  
  mode <- match.arg(mode)
  
  # Création du plan d'échantillonnage pondéré
  design <- svydesign(ids = ~1, data = donnees, weights = ~poids_tirage)
  
  # Estimation du modèle logistique pondéré
  modele <- svyglm(formula = formule_cnr,
                   design = design,
                   family = quasibinomial(link = "logit"))
  
  # Prédiction des probabilités de réponse issues du modèle
  proba_rep <- predict(modele, type = "response")
  proba_rep <- as.numeric(proba_rep)
  
  # Colonnes spécifiques au mode
  rep_col <- if (mode == "mono") paste0("rep_mono_", scenario) else paste0("rep_multi_", scenario)
  
  # Calcul des poids CNR exacts et de la proba réelle (exacte)
  if (mode == "mono") {
    variable <- paste0("pi_int_", scenario)
    
    donnees[[paste0("poids_cnr_exacte_", scenario)]] <- donnees[[paste0("rep_", scenario)]] * donnees$poids_tirage * (1 / donnees[[variable]])
    donnees[[paste0("proba_cnr_exacte_", scenario)]] <- donnees[[paste0("rep_", scenario)]] * donnees[[variable]]
    
  } else if (mode == "multi") {
    variable_int <- paste0("pi_int_", scenario)
    variable_tel_non_int <- paste0("pi_tel_non_int_", scenario)
    
    pi_exacte <- donnees[[variable_int]] + (1 - donnees[[variable_int]]) * donnees[[variable_tel_non_int]]
    donnees[[paste0("poids_cnr_exacte_", scenario)]] <- donnees$poids_tirage * (1 / pi_exacte) * donnees[[paste0("rep_", scenario)]]
    donnees[[paste0("proba_cnr_exacte_", scenario)]] <- pi_exacte * donnees[[paste0("rep_", scenario)]]
  }
  
  # Poids CNR sans GRH (modèle logistique)
  donnees[[paste0("poids_cnr_sans_grh_", scenario)]] <- (donnees$poids_tirage / proba_rep) * donnees[[paste0("rep_", scenario)]]
  donnees[[paste0("proba_cnr_sans_grh_", scenario)]] <- proba_rep * donnees[[paste0("rep_", scenario)]]
  
  # Si GRH activé
  if (!is.null(grh)) {
    nom_var_rep <- all.vars(update(formule_cnr, . ~ NULL))
    
    resultat_grh <- ghr_quantile(
      proba_estimees = proba_rep,
      ind_rep = donnees[[nom_var_rep]],
      poids_tirage = donnees$poids_tirage,
      grh = grh,
      taux_min_grh = taux_min_grh
    )
    
    donnees[[paste0("grh_groupe_", scenario)]] <- resultat_grh$grh_groupe
    donnees[[paste0("poids_cnr_avec_grh_", scenario)]] <- (donnees$poids_tirage / resultat_grh$proba_estimees) * donnees[[paste0("rep_", scenario)]]
    donnees[[paste0("proba_cnr_avec_grh_", scenario)]] <- resultat_grh$proba_estimees * donnees[[paste0("rep_", scenario)]]
  }
  
  # Retourner le modèle aussi pour pouvoir faire des prédictions sur d'autres données
  return(list(donnees = donnees, modele = modele))
}