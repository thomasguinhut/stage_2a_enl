resultats <- lancer_une_simulation(
  i = 1,
  bdd = bdd,
  sigma = sigma_1,
  modele_latent = TRUE,
  formule_cnr = formule_cnr,
  nom_methodes = nom_methodes,
  n_multi = n_multi,
  n_mono = n_mono,
  part_strate_A_dans_mode_1 = 1 - part_idf_multimode,
  part_strate_A_dans_mode_2 = 1 - part_idf_monomode,
  scenarios = scenarios_nr,
  prefix_var_interet = prefix_var_interet,
  strat_var_name = "strate_vec",
  grh = grh,
  taux_min_grh = taux_min_grh
)