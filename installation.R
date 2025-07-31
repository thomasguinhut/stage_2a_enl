setwd("~/work/stage_2a_enl")


################################# Étape A ######################################
############### PRÉPARATION DES DONNÉES ET DE L'ENVIRONNEMENT ##################


source("A-donnees/A3-importation/A3.1-importations_donnees.R")
source("A-donnees/A3-importation/A3.2-chargement_environnement.R")
source("A-donnees/A3-importation/A3.3-chargement_theme_rstudio.R")
chargement_theme_rstudio()



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