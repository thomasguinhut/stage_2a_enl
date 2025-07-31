bdd_avec_tirage <- tirage(donnees = bdd,
                          n_multi = n_multi,
                          n_mono = n_mono,
                          part_strate_A_dans_mode_1 = 1 - part_idf_multimode,
                          part_strate_A_dans_mode_2 = 1 - part_idf_monomode,
                          scenarios = scenarios_nr,
                          sigma = sigma_1,
                          strat_var_name = strat_var_name)

################################ VERIFICATION ##################################

# # On a bien les 13 colonnes supplémentaires désirées
# setdiff(colnames(bdd_avec_tirage), colnames(bdd))
# 
# # Nombre de personnes tirées
# nrow(donnees_avec_tirage[donnees_avec_tirage$ind_tirage == 1, ])
# 
# # Nombre de personnes ayant répondu
# nrow(donnees_avec_tirage[donnees_avec_tirage$rep_1 == 1, ])
# 
# # Répartition des répondants par croisement mode x strate (Idf vs Rdf)
# donnees_avec_tirage[ind_tirage == 1, .N, .(strate_vec, ind_tirage_multimode)]
# 
# # Somme des poids : on retrouve 1M dans les deux cas
# donnees_avec_tirage[ind_tirage_multimode == 1, sum(poids_tirage)]
# donnees_avec_tirage[ind_tirage_monomode == 1, sum(poids_tirage)]
# 
# # On retrouve environ 60% en multimode de taux de réponse et environ 40 en monomode
# donnees_avec_tirage[ind_tirage == 1, round(mean(rep_1) * 100), ind_tirage_multimode]
