taux_rep_grh <- function(df_taux_rep, nb_sim, nom_dossier, num_dossier, sc, chemin_sous_dossier, aws) {
  
  # Filtrer le scénario demandé
  df_taux_rep <- df_taux_rep %>% filter(scenario == !!sc)
  
  # Trier grh numériquement
  df_taux_rep$grh <- factor(df_taux_rep$grh, levels = unique(sort(as.integer(df_taux_rep$grh))))
  
  # Ordre de mode : global avant multi avant mono
  niveau_mode <- c("global", "multi", "mono")
  df_taux_rep$mode <- factor(df_taux_rep$mode, levels = niveau_mode)
  
  # Couleurs sobres : rouge, bleu, vert
  couleurs <- c(
    "global" = "#3B5998",  # rouge brique doux
    "multi"  = "#B7413E",  # bleu marine doux
    "mono"   = "#4E7D49"   # vert forêt doux
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
    theme_classic() +
    labs(
      x = paste0("\nGRH communs aux ", nb_sim, " simulations réalisées"),
      y = paste0("Taux de réponse moyen (%) avec le scénario ", sc, " de NR\n"),
      fill = "Échantillon"
    )
  
  export_pdf(
    plot = plot,
    filename = paste0(chemin_sous_dossier, "/5-taux_rep_grh_scenario_", sc, ".pdf"),
    width = 8.27,
    height = 5.83,
    aws = aws,
    chemin_sous_dossier = chemin_sous_dossier
  )
  
}
