################################# Étape C ######################################
############## APPLICATION DES TIRAGES, ESTIMATIONS ET EXPORTATIONS ############


# ---------------------------- TESTS -------------------------------------------

source("R/C-application/C.1-tirage_simple.R")
source("R/C-application/C.2-cnr.R")
source("R/C-application/C.3-estimations.R")
source("R/C-application/C.4-resultats_un_echantillon.R")
test <- resultats$brut



# ------------------------- SIMULATIONS ----------------------------------------

resultats <- boucles_simulations(
  nb_sim = nb_sim,
  bdd = bdd,
  nom_methodes = nom_methodes,
  n_multi = n_multi,
  n_mono = n_mono,
  part_strate_A_dans_mode_1 = 1 - part_idf_multimode,
  part_strate_A_dans_mode_2 = 1 - part_idf_monomode,
  scenarios = scenarios_nr,
  prefix_var_interet = prefix_var_interet,
  strat_var_name = strat_var_name,
  sigma = sigma_1,
  modele_latent = TRUE,
  formule_cnr = formule_cnr,
  grh = grh,
  taux_min_grh = taux_min_grh,
  parallel = TRUE,
  n_cores = 12
)


# ---------------------- IMPORTS / EXPORTS -------------------------------------

nom_dossier <- "scenario_4"
num_dossier <- "4"

# Créer le dossier s’il n’existe pas
chemin_dossier <- paste0("stage_2a_enl/exports/", num_dossier, "-", nom_dossier)

# Sauvegarde
# export_resultats(resultats, num_dossier, chemin_dossier, nom_dossier)

# Réimportation des résultats dans l'environnement de travail
lire_resultats(num_dossier, nom_dossier, chemin_dossier, scenarios_nr, nom_methodes, prefix_var_interet, nb_sim)

# Appels dynamiques des fonctions de post-traitement pour chaque scénario
for (sc in scenarios_nr) {
  num_sous_dossier <- 4 + as.integer(sc) - 1
  chemin_sous_dossier_aws <- paste0("s3/thomasguinhut/stage_2a_enl/exports/", num_dossier, "-", nom_dossier, "/graphiques_scenario_", sc)
  chemin_sous_dossier_D <- paste0("R/D-exports/D", num_dossier, "-", nom_dossier, "/", "D", num_dossier, ".", num_sous_dossier, "-graphiques_scenario_", sc)

  dir.create(chemin_sous_dossier_D, recursive = TRUE, showWarnings = FALSE)

  # Récupération des objets
  obj_biais <- get(paste0(nom_dossier, "_biais"), envir = .GlobalEnv)
  obj_brut <- get(paste0(nom_dossier, "_brut"), envir = .GlobalEnv)
  obj_taux <- get(paste0(nom_dossier, "_taux_rep_grh"), envir = .GlobalEnv)

  # Graphiques et tableau
  tableau_resultats(obj_brut, nom_dossier, num_dossier, sc, chemin_sous_dossier_aws, chemin_sous_dossier_D)
  graphique_principal(obj_biais, nb_sim, nom_methodes, nom_dossier, num_dossier, sc, chemin_sous_dossier_aws, chemin_sous_dossier_D, "boxplots")
  graphique_principal(obj_biais, nb_sim, nom_methodes, nom_dossier, num_dossier, sc, chemin_sous_dossier_aws, chemin_sous_dossier_D, "biais", xmax_biais = 8.5)
  graphique_principal(obj_biais, nb_sim, nom_methodes, nom_dossier, num_dossier, sc, chemin_sous_dossier_aws, chemin_sous_dossier_D, "eqm", xmax_eqm = 8.5)
  taux_rep_grh(obj_taux, nb_sim, nom_dossier, num_dossier, sc, chemin_sous_dossier_aws, chemin_sous_dossier_D)
  # comparaisons_coef_hartley(obj_biais, n_multi, n_mono)

  # Nettoyage
  rm(obj_biais, obj_brut, obj_taux)

  cat(paste0(
    "Tous les tableaux et figures du scénario ", sc, " ont bien été exportés dans le dossier correspondant.\n"
  ))
  
  unlink("R/D-exports", recursive = TRUE, force = TRUE)
  
}
  