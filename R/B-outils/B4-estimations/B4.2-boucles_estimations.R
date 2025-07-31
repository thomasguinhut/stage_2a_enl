source("B-outils/B4-estimations/B4.1-calcul_totaux.R")
lapply(list.files("B-outils/B3-estimateurs", pattern = "\\.R$", full.names = TRUE), source) # Importation des estimateurs


boucles_estimations <- function(
    bdd, 
    nom_methodes,
    n_multi,
    n_mono,
    part_strate_A_dans_mode_1,
    part_strate_A_dans_mode_2,
    nom_var_prob_internet,
    nom_var_prob_tel,
    scenarios,
    prefix_var_interet,
    strat_var_name,
    sigma,
    modele_latent,
    formule_cnr,
    grh,
    taux_min_grh
) {
  
  # Étape 1 : tirage des échantillons et simulation des répondants
  bdd_avec_tirage <- tirage(
    donnees = bdd,
    n_multi = n_multi,
    n_mono = n_mono,
    part_strate_A_dans_mode_1 = part_strate_A_dans_mode_1,
    part_strate_A_dans_mode_2 = part_strate_A_dans_mode_2,
    nom_var_prob_internet = nom_var_prob_internet,
    nom_var_prob_tel = nom_var_prob_tel,
    scenarios = scenarios,
    sigma = sigma,
    strat_var_name = strat_var_name,
    modele_latent = modele_latent
  )
  
  # Étape 2 : calcul des pondérations CNR
  bdd_avec_tirage_et_cnr <- cnr_finale(
    donnees = bdd_avec_tirage,
    formule_cnr = formule_cnr,
    scenarios = scenarios,
    grh = grh,
    taux_min_grh = taux_min_grh
  )
  
  resultats <- list()
  
  for (scenario in scenarios) {
    
    donnees_temp <- copy(bdd_avec_tirage_et_cnr)
    
    variantes <- c("cnr_exacte", "cnr_sans_grh", "cnr_avec_grh")
    
    # Appliquer les méthodes de pondération demandées
    donnees_temp <- estimateurs_HT_classiques(donnees = donnees_temp, scenarios = scenario, variantes = variantes)
    
    for (methode in nom_methodes) {
      nom_fonction <- paste0("estimateur_", methode)
      
      if (exists(nom_fonction, mode = "function")) {
        donnees_temp <- get(nom_fonction)(donnees = donnees_temp, scenarios = scenario, variantes = variantes)
      } else {
        warning(paste("La fonction", nom_fonction, "n'existe pas."))
      }
    }
    
    # Estimations pondérées
    res <- calcul_totaux(donnees_temp,
                         nom_var_rep = paste0("rep_", scenario),
                         prefix_var_interet = prefix_var_interet,
                         nom_methodes = nom_methodes)
    
    res$scenario <- scenario
    resultats[[scenario]] <- res
  }
  
  # Fusion des résultats
  resultats_final <- data.table::rbindlist(resultats, use.names = TRUE, fill = TRUE) %>%
    mutate(
      y = str_remove(variable_interet, "strate_[AB]$|total$"),
      
      scenario_nr = ifelse(str_detect(variable_ponderation, "_[0-9]+$"),
                           str_extract(variable_ponderation, "[0-9]+$"),
                           NA),
      
      ensemble = case_when(
        str_detect(variable_interet, "strate_A$") ~ "strate_A",
        str_detect(variable_interet, "strate_B$") ~ "strate_B",
        TRUE ~ "total"
      ),
      
      # Bloc dynamique pour identifier l'estimateur
      
      estimateur = case_when(
        str_detect(variable_ponderation, "recensement") ~ "recensement",
        str_detect(variable_ponderation, "mono") ~ "HT_mono",
        str_detect(variable_ponderation, "multi") ~ "HT_multi",
        variable_ponderation == "poids_3_sans_nr" ~ "3",
        variable_ponderation == "poids_3prime_sans_nr" ~ "3prime",
        !!!setNames(
          lapply(nom_methodes[order(-nchar(nom_methodes))], function(m) {
            rlang::expr(str_detect(variable_ponderation, !!m) ~ !!m)
          }),
          NULL
        ),
        TRUE ~ "HT"
      ),
      
      # Bloc dynamique pour la méthode
      methode = case_when(
        str_detect(variable_ponderation, "sans_nr") ~ "sans_nr",
        str_detect(variable_ponderation, "cnr_exacte") ~ "cnr_exacte",
        str_detect(variable_ponderation, "sans_grh") ~ "sans_grh",
        str_detect(variable_ponderation, "avec_grh") ~ "avec_grh",
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(
      ensemble = factor(ensemble, levels = c("total", "strate_A", "strate_B")),
      nom = paste0(variable_interet, "_", variable_ponderation)
    ) %>%
    select(scenario_nr, y, ensemble, methode, estimateur, somme_ponderee, nom) %>%
    arrange(
      desc(is.na(scenario_nr)), scenario_nr,
      y,
      ensemble,
      desc(is.na(methode)), methode,
      estimateur
    ) %>%
    distinct(nom, .keep_all = TRUE)
  
  return(list(
    resultats = resultats_final,
    bdd_avec_tirage_et_cnr = bdd_avec_tirage_et_cnr
  ))
}
