comparaisons_coef_hartley <- function(df, n_mono, n_multi) {
  df_ratio <- df %>%
    filter(estimateur %in% c("HT_mono", "HT_multi")) %>%  # plus de filtre sur ensemble
    group_by(y, ensemble, estimateur) %>%
    summarise(var_somme = var(somme_ponderee), .groups = "drop") %>%
    pivot_wider(names_from = estimateur, values_from = var_somme) %>%
    mutate(ratio = HT_mono / (HT_mono + HT_multi)) %>%
    select(y, ensemble, ratio)
  
  df_result <- df_ratio %>%
    pivot_wider(names_from = y, values_from = ratio)
  
  print(df_result)
  
  cat(paste0("\nPartage de poids : ", round(1 / 2, 3), "\n"))
  cat(paste0("Poids relatif des échantillons (estimateur 1a) : ", round(n_mono / (n_mono + n_multi), 3), "\n"))
}
