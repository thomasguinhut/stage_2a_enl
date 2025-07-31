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
