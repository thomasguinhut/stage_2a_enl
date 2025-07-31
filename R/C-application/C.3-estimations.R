variantes <- c("cnr_exacte", "cnr_sans_grh", "cnr_avec_grh")

# Appliquer la première fonction explicitement
bdd_avec_tirage_et_cnr_et_combi <- estimateurs_HT_classiques(
  donnees = bdd_avec_tirage_et_cnr,
  scenarios = scenarios_nr,
  variantes = variantes
)

# Liste tous les estimateurs sauf la fonction initiale
all_funs <- ls(envir = globalenv())
fct_estimateurs <- grep("^estimateur(?!(_hartley|_elliott))", all_funs, perl = TRUE, value = TRUE)

# Supprimer "estimateurs_HT_classiques" s'il est là
fct_estimateurs <- setdiff(fct_estimateurs, "estimateurs_HT_classiques")
fct_estimateurs <- sort(fct_estimateurs)  # ou trie à ta façon

# Appliquer les estimateurs en chaîne
for (f in fct_estimateurs) {
  bdd_avec_tirage_et_cnr_et_combi <- do.call(
    f,
    list(
      donnees = bdd_avec_tirage_et_cnr_et_combi,
      scenarios = scenarios_nr,
      variantes = variantes
    )
  )
}

rm(variantes)

################################ VERIFICATION ##################################

# On a bien les 4 colonnes supplémentaires désirées
# setdiff(colnames(bdd_avec_tirage_et_cnr_et_combi), colnames(bdd_avec_tirage_et_cnr)

