devtools::install_github("max-alletsee/rstudio-themes")

################################# Étape A ######################################
############### PRÉPARATION DES DONNÉES ET DE L'ENVIRONNEMENT ##################


# Importation des données simulées prêtes à l'emploi
source("A-donnees/A3-importation/A3.1-importations_donnees.R")

# Chargement des packages et nettoyage partiel de l’environnement
source("A-donnees/A3-importation/A3.2-chargement_environnement.R")



################################# Étape B ######################################
######################## CHARGEMENT DES FONCTIONS ##############################


source("B-outils/B1-tirage/B1.5-tirage.R")
source("B-outils/B2-correction_non_reponse/B2.3-cnr_finale.R")
source("B-outils/B4-estimations/B4.4-boucles_simulations.R")

source("B-outils/B5-export_import/B5.1-export_resultats.R")
source("B-outils/B5-export_import/B5.2-lire_resultats.R")
source("B-outils/B5-export_import/B5.3-export_pdf.R")

source("B-outils/B6-tableaux_figures/B6.1-tableau_resultats.R")
source("B-outils/B6-tableaux_figures/B6.2-graphique_principal.R")
source("B-outils/B6-tableaux_figures/B6.3-taux_rep_grh.R")
source("B-outils/B6-tableaux_figures/B6.4-comparaisons_coef_hartley.R")



################################# Étape C ######################################
############## APPLICATION DES TIRAGES, ESTIMATIONS ET EXPORTATIONS ############


# ------------------------ À PARAMÉTRER ----------------------------------------

nb_sim <- 100
n_multi <- 30800
part_idf_multimode <- 0.148
n_mono <- 50000
part_idf_monomode <- 0.49562
grh <- 20
taux_min_grh = 0.1

scenarios_nr <- c("1")
prefix_var_interet <- c("y_1_", "y_2_", "y_3_")
nom_methodes <- c("1a", "1aprime", "1b", "2a", "2aprime", "3a", "3aprime", "3b", "3bprime", "4")
formule_cnr = "x_1 + x_2 + x_3 + x_4 + x_5"
strat_var_name = "strate_vec"
aws = TRUE



# ---------------------------- TESTS -------------------------------------------

source("C-application/C.1-tirage_simple.R")
source("C-application/C.2-cnr.R")
source("C-application/C.3-estimations.R")
source("C-application/C.4-resultats_un_echantillon.R")
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
  parallel = TRUE # à mettre que si nb_sim est grand, sinon faire séquentiellement (parallel = FALSE) est plus efficace
)


# ---------------------- IMPORTS / EXPORTS -------------------------------------

nom_dossier <- "test"
num_dossier <- "0"

# Créer le dossier s’il n’existe pas
chemin_dossier <- ifelse(aws == FALSE, paste0("D-exports/D", num_dossier, "-", nom_dossier),  paste0("stage_2a_enl/exports/", num_dossier, "-", nom_dossier))
# dir.create(chemin_dossier, recursive = TRUE, showWarnings = FALSE)

# Sauvegarde
export_resultats(resultats, num_dossier, chemin_dossier, nom_dossier, aws)

# Réimportation des résultats dans l'environnement de travail
lire_resultats(num_dossier, nom_dossier, chemin_dossier, scenarios_nr, nom_methodes, prefix_var_interet, nb_sim, aws)

# Appels dynamiques des fonctions de post-traitement pour chaque scénario
for (sc in scenarios_nr) {

  num_sous_dossier <- 4 + as.integer(sc) - 1
  chemin_sous_dossier <- paste0("D-exports/D", num_dossier, "-", nom_dossier, "/", "D", num_dossier, ".", num_sous_dossier, "-graphiques_scenario_", sc)
  
  dir.create(chemin_sous_dossier, recursive = TRUE, showWarnings = FALSE)
  
  # Récupération des objets
  obj_biais <- get(paste0(nom_dossier, "_biais"), envir = .GlobalEnv)
  obj_brut <- get(paste0(nom_dossier, "_brut"), envir = .GlobalEnv)
  obj_taux <- get(paste0(nom_dossier, "_taux_rep_grh"), envir = .GlobalEnv)
  
  # Graphiques et tableau
  tableau_resultats(obj_brut, nom_dossier, num_dossier, sc, chemin_sous_dossier, aws = FALSE)
  graphique_principal(obj_biais, nb_sim, nom_methodes, nom_dossier, num_dossier, sc, chemin_sous_dossier, "boxplots", aws = FALSE)
  # graphique_principal(obj_biais, nb_sim, nom_methodes, nom_dossier, num_dossier, sc, chemin_sous_dossier, "biais", aws = FALSE)
  # graphique_principal(obj_biais, nb_sim, nom_methodes, nom_dossier, num_dossier, sc, chemin_sous_dossier, "eqm", aws = FALSE)
  # taux_rep_grh(obj_taux, nb_sim, nom_dossier, num_dossier, sc, chemin_sous_dossier, aws = FALSE)
  # comparaisons_coef_hartley(obj_biais, n_multi, n_mono)
  
  # Nettoyage
  rm(obj_biais, obj_brut, obj_taux)
  
  cat(paste0(
    "\nTous les tableaux et figures du scénario ", sc ," ont bien été exportés dans le dossier correspondant.\n"))
}


################################ NETTOYAGE #####################################

# Nettoyage de l'environnement
rm(list = ls())

# Fermeture de toutes les fenêtres graphiques ouvertes
while (dev.cur() > 1) dev.off()




class(test_biais)


names(bdd_avec_tirage_et_cnr_et_combi)



mean(bdd_avec_tirage_et_cnr_et_combi$ind_tirage_monomode[bdd_avec_tirage_et_cnr_et_combi$ind_tirage_monomode == 1])

str(bdd_avec_tirage_et_cnr)
