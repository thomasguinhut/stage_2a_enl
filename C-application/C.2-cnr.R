bdd_avec_tirage_et_cnr <-  cnr_finale(donnees = bdd_avec_tirage,
                                      formule_cnr = formule_cnr,
                                      scenarios = scenarios_nr,
                                      grh = grh,
                                      taux_min_grh = taux_min_grh)

################################ VERIFICATION ##################################

# On a bien les 2 colonnes supplémentaires désirées
# setdiff(colnames(bdd_avec_tirage_et_cnr), colnames(bdd_avec_tirage))
