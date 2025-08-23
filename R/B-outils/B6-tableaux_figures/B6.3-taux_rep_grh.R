taux_rep_grh <- function(df_taux_rep, nb_sim, nom_dossier, num_dossier, sc, chemin_sous_dossier_aws, chemin_sous_dossier_D) {
  
  # Filtrer le scénario demandé
  df_taux_rep <- df_taux_rep %>% filter(scenario == !!sc)
  
  # Trier grh numériquement
  df_taux_rep$grh <- factor(df_taux_rep$grh, levels = unique(sort(as.integer(df_taux_rep$grh))))
  
  # Ordre de mode : global avant multi avant mono
  niveau_mode <- c("global", "multi", "mono")
  df_taux_rep$mode <- factor(df_taux_rep$mode, levels = niveau_mode)
 
  # Couleurs : dégradé de bleu pour global/multi/mono
  couleurs <- c(
    "global" = "#193e61",  # bleu moyen (global)
    "multi"  = "#7FB3D3",  # bleu clair (multi)
    "mono"   = "#195f90"   # bleu foncé (mono)
  )
  
  plot <- df_taux_rep %>%
    group_by(grh, mode) %>%
    summarise(
      mean_taux = mean(taux_rep, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = grh, y = mean_taux, fill = mode)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_fill_manual(values = couleurs) +
    scale_y_continuous(breaks = seq(0, 100, by = 10)) +  # Trait tous les 10 sur l'axe y
    theme_classic() +
    labs(
      x = paste0("\nGRH communs aux 10 000 simulations réalisées"),
      y = paste0("Taux de réponse moyen (%) avec le scénario ", sc, " de réponse\n"),
      fill = "Échantillon"
    )
  
  nom_fichier_D <- paste0(chemin_sous_dossier_D, "/5-taux_rep_grh_scenario_", sc, ".pdf")
  nom_fichier_aws <- paste0(chemin_sous_dossier_aws, "/5-taux_rep_grh_scenario_", sc, ".pdf")
  ggsave(nom_fichier_D, plot = plot, width = 8.27, height = 5.83, device = cairo_pdf)
  system(paste0("mc cp ", nom_fichier_D, " ", nom_fichier_aws), intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
}
