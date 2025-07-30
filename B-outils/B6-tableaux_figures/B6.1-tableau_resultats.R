tableau_resultats <- function(brut, nom_dossier, num_dossier, sc, chemin_sous_dossier, aws){
  
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

  if (aws == FALSE) {
    
    write.csv(tableau, paste0(chemin_sous_dossier, "/1-tableau_resultats_", nom_dossier, "_scenario_", sc, ".csv"))
  
  } else {
    
    aws.s3::s3write_using(
      tableau,
      FUN = readr::write_csv,
      object = paste0(chemin_sous_dossier, "/1-tableau_resultats_", nom_dossier, "_scenario_", sc, ".csv"),
      bucket = "thomasguinhut",
      opts = list("region" = "")
    )
    
  }
}
