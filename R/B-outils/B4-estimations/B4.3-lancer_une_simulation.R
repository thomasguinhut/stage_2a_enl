source("R/B-outils/B4-estimations/B4.2-boucles_estimations.R")

lancer_une_simulation <- function(i,
                                  bdd,
                                  nom_methodes,
                                  n_multi,
                                  n_mono,
                                  part_strate_A_dans_mode_1,
                                  part_strate_A_dans_mode_2,
                                  scenarios,
                                  prefix_var_interet,
                                  strat_var_name,
                                  sigma,
                                  modele_latent,
                                  formule_cnr,
                                  grh,
                                  taux_min_grh) {
  res_list <- boucles_estimations(
    bdd = bdd,
    nom_methodes = nom_methodes,
    n_multi = n_multi,
    n_mono = n_mono,
    part_strate_A_dans_mode_1 = part_strate_A_dans_mode_1,
    part_strate_A_dans_mode_2 = part_strate_A_dans_mode_2,
    scenarios = scenarios,
    prefix_var_interet = prefix_var_interet,
    strat_var_name = strat_var_name,
    sigma = sigma,
    modele_latent = modele_latent,
    formule_cnr = formule_cnr,
    grh = grh,
    taux_min_grh = taux_min_grh
  )

  brut <- res_list$resultats %>%
    dplyr::mutate(simulation = i)

  if (is.null(res_list$resultats) || nrow(res_list$resultats) == 0) {
    warning(sprintf("Attention : 'resultats' vide ou NULL pour la simulation %d", i))
  }

  bdd_avec_tirage_et_cnr <- res_list$bdd_avec_tirage_et_cnr

  # Initialisation de la table de sortie
  taux_rep_grh_mode <- data.frame()

  # Définition des colonnes spécifiques pour chaque mode
  mode_config <- list(
    mono = list(
      rep = "rep_mono",
      tir = "ind_tirage_monomode"
    ),
    multi = list(
      rep = "rep_multi",
      tir = "ind_tirage_multimode"
    ),
    global = list(
      rep = "rep",
      tir = "ind_tirage"
    )
  )

  # Boucle sur les scénarios
  for (s in scenarios) {
    grh_col <- paste0("grh_groupe_", s)

    # Boucle sur les modes
    for (mode_name in names(mode_config)) {
      rep_var <- paste0(mode_config[[mode_name]]$rep, "_", s)
      tir_var <- mode_config[[mode_name]]$tir

      # Boucle sur chaque modalité de GRH
      for (g in 1:grh) {
        sous_bdd <- bdd_avec_tirage_et_cnr[bdd_avec_tirage_et_cnr[[grh_col]] == g, ]
        numerateur <- sum(sous_bdd[[rep_var]])
        denominateur <- sum(sous_bdd[[tir_var]])

        taux <- ifelse(denominateur > 0, numerateur / denominateur, NA)

        df_tmp <- data.frame(
          simulation = i,
          scenario = s,
          grh = g,
          mode = mode_name,
          taux_rep = round(100 * taux, 2)
        )

        taux_rep_grh_mode <- rbind(taux_rep_grh_mode, df_tmp)
      }
    }
  }

  # Nettoyage
  rownames(taux_rep_grh_mode) <- NULL




  bdd_tire <- bdd_avec_tirage_et_cnr[bdd_avec_tirage_et_cnr$ind_tirage_multimode == 1 | bdd_avec_tirage_et_cnr$ind_tirage_monomode == 1, ]

  poids_moyens <- data.frame()

  if (nrow(bdd_tire) > 0) {
    # Déterminer le mode de façon vectorielle
    bdd_tire$mode <- ifelse(
      bdd_tire$ind_tirage_multimode == 1 & bdd_tire$ind_tirage_monomode == 1,
      "multi_et_mono",
      ifelse(bdd_tire$ind_tirage_multimode == 1, "multi", "mono")
    )

    # Créer une liste pour stocker les résultats par scénario
    poids_list <- lapply(scenarios, function(s) {
      poids_cnr_exacte_col <- paste0("poids_cnr_exacte_", s)
      poids_cnr_sans_grh_col <- paste0("poids_cnr_sans_grh_", s)
      poids_cnr_avec_grh_col <- paste0("poids_cnr_avec_grh_", s)

      data.frame(
        id_ind = c(1:(n_multi + n_mono)),
        simulation = i,
        mode = bdd_tire$mode,
        scenario = s,
        poids_cnr_exacte = if (poids_cnr_exacte_col %in% names(bdd_tire)) bdd_tire[[poids_cnr_exacte_col]] else NA_real_,
        poids_cnr_sans_grh = if (poids_cnr_sans_grh_col %in% names(bdd_tire)) bdd_tire[[poids_cnr_sans_grh_col]] else NA_real_,
        poids_cnr_avec_grh = if (poids_cnr_avec_grh_col %in% names(bdd_tire)) bdd_tire[[poids_cnr_avec_grh_col]] else NA_real_
      )
    })

    poids_cnr <- dplyr::bind_rows(poids_list)
  }


  list(
    brut = brut,
    bdd_avec_tirage_et_cnr = bdd_avec_tirage_et_cnr,
    taux_rep_grh_mode = taux_rep_grh_mode,
    poids_cnr = poids_cnr
  )
}
