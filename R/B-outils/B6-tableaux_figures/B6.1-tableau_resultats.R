tableau_resultats <- function(brut, nom_dossier, num_dossier, sc, chemin_sous_dossier_aws, chemin_sous_dossier_D){
  
  brut <- brut %>%
    filter(is.na(scenario_nr) | scenario_nr == sc)
  
  tableau <- brut %>% 
    filter(estimateur != "recensement") %>% 
    group_by(y, methode, estimateur, ensemble) %>% 
    summarise(moyenne = round(mean(somme_ponderee)),
              variance = round(var(somme_ponderee)),
              .groups = "drop") %>%
    left_join(
      brut %>% 
        filter(estimateur == "recensement") %>% 
        select(y, ensemble, esperance = somme_ponderee),
      by = c("y", "ensemble")
    ) %>%
    mutate(esperance = round(esperance),
           biais = moyenne - esperance,
           biais_relatif = round(((biais) / esperance) * 100, 10),
           biais_relatif_abs = round(abs((biais) / esperance) * 100, 10),
           eqm = variance + biais^2,
           reqm = sqrt(eqm),
           cv = round(sqrt(variance) / esperance * 100, 10),
           cv_reqm = round(reqm / esperance * 100, 10))
  
  nom_fichier_D <- paste0(chemin_sous_dossier_D, "/1-tableau_resultats_scenario_", sc, ".csv")
  nom_fichier_aws <- paste0(chemin_sous_dossier_aws, "/1-tableau_resultats_scenario_", sc, ".csv")
  write.csv(tableau, nom_fichier_D)
  
  system(paste0("mc cp ", nom_fichier_D, " ", nom_fichier_aws), intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)

}
