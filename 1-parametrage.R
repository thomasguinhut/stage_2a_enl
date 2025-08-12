################################# Étape C ######################################
############################### PARAMÉTRAGE ####################################


nb_sim <- 10000
n_multi <- 30800
part_idf_multimode <- 0.148
n_mono <- 50000
part_idf_monomode <- 0.49562
grh <- 20
taux_min_grh <- 0.1

scenarios_nr <- c("1", "2", "3", "4")
prefix_var_interet <- c("y_1_", "y_3_", "y_2_")
nom_methodes <- c("1a", "1aprime", "1b", "2a", "2aprime", "3a", "3aprime", "3b", "3bprime", "4")
formule_cnr <- "x_1 + x_2 + x_3 + x_4 + x_5"
strat_var_name <- "strate_vec"