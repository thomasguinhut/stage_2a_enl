################################# Étape A ######################################
############### PRÉPARATION DES DONNÉES ET DE L'ENVIRONNEMENT ##################


source("R/A-donnees/A3-importation/A3.1-chargement_environnement.R")
source("R/A-donnees/A3-importation/A3.2-chargement_theme_rstudio.R")
source("R/A-donnees/A3-importation/A3.3-importations_donnees.R")


################################# Étape B ######################################
######################## CHARGEMENT DES FONCTIONS ##############################


source("R/B-outils/B1-tirage/B1.5-tirage.R")
source("R/B-outils/B2-correction_non_reponse/B2.3-cnr_finale.R")
source("R/B-outils/B4-estimations/B4.4-boucles_simulations.R")

source("R/B-outils/B5-export_import/B5.1-export_resultats.R")
source("R/B-outils/B5-export_import/B5.2-lire_resultats.R")

source("R/B-outils/B6-tableaux_figures/B6.1-tableau_resultats.R")
source("R/B-outils/B6-tableaux_figures/B6.2-graphique_principal.R")
source("R/B-outils/B6-tableaux_figures/B6.3-taux_rep_grh.R")
source("R/B-outils/B6-tableaux_figures/B6.4-comparaisons_coef_hartley.R")



################################# Étape C ######################################
############################### PARAMÉTRAGE ####################################


nb_sim <- 50
n_multi <- 30800
part_idf_multimode <- 0.148
n_mono <- 50000
part_idf_monomode <- 0.49562
grh <- 20
taux_min_grh <- 0.1

scenarios_nr <- c("1", "2", "3", "4")
prefix_var_interet <- c("y_1_", "y_2_", "y_3_")
nom_methodes <- c("1a", "1aprime", "1b", "2a", "2aprime", "3a", "3aprime", "3b", "3bprime", "4")
formule_cnr <- "x_1 + x_2 + x_3 + x_4 + x_5"
strat_var_name <- "strate_vec"
