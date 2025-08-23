meilleurs_estimateurs <- function(numero) {
  
  df <- aws.s3::s3read_using(
    FUN = read_csv,
    object = paste0("stage_2a_enl/exports/", numero, "-scenario_", numero, "/graphiques_scenario_", numero, "/1-tableau_resultats_scenario_", numero, "_scenario_", numero, ".csv"),
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )
  
  df <- df[,-1]
  df <- df[,c(1,2,3,4,14)]
  df <- df %>% 
    filter(methode %in% c("avec_grh", "sans_grh"))
  
  df <- df %>% 
    group_by(methode, ensemble, estimateur) %>% 
    summarise(cv = mean(cv_reqm), .groups = "drop")
  
  df <- df %>%
    filter(!estimateur %in% c("monomode_expansion", "multimode_expansion")) %>%
    left_join(
      df %>%
        filter(estimateur %in% c("monomode_expansion", "multimode_expansion")) %>%
        pivot_wider(names_from = estimateur, values_from = cv) %>%
        rename(cv_mono = monomode_expansion, cv_multi = multimode_expansion),
      by = c("methode", "ensemble")
    ) %>%
    mutate(
      diff_cv_multi = cv - cv_multi,
      diff_cv_mono = cv - cv_mono
    ) %>%
    select(-cv_mono, -cv_multi, -cv)
  
  df <- df %>%
    mutate(min_diff = pmin(diff_cv_multi, diff_cv_mono)) %>%
    group_by(methode, ensemble) %>% 
    slice_min(order_by = min_diff, n = 1) %>% 
    select(-min_diff) %>%
    mutate(diff_cv_multi = round(diff_cv_multi * 100),
           diff_cv_mono = round(diff_cv_mono * 100))
  
  print(df)
}