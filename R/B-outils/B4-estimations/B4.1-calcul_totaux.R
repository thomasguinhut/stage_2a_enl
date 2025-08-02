calcul_totaux <- function(table_post_sim, 
                          prefix_var_interet, 
                          nom_var_rep,
                          nom_methodes) {
  
  # Extraire le suffixe à partir de nom_var_rep (donne "_1", "_2", etc.)
  suffixe <- sub("rep", "", nom_var_rep)
  
  # Initialiser les poids fixes
  poids_fixes <- c(
    "recensement",
    "poids_tirage_multimode",
    "poids_tirage_monomode"
  )
  
  # Ajouter les poids selon les méthodes présentes
  if (any(c("3a", "3aprime", "3b", "3bprime") %in% nom_methodes)) {
    poids_a_ajouter <- c(
      if (any(c("3a", "3b") %in% nom_methodes)) "poids_3_sans_nr",
      if (any(c("3aprime", "3bprime") %in% nom_methodes)) "poids_3prime_sans_nr"
    )
    
    poids_fixes <- c(poids_fixes, poids_a_ajouter)
  }
  
  # Générer dynamiquement les poids selon les noms de scénarios
  poids_dynamiques <- paste0("poids_", nom_methodes, "_sans_nr")
  
  # Combiner les deux
  poids_fixes <- c(poids_fixes, poids_dynamiques)

  # On garde que ceux qui existent vraiment dans la table
  poids_fixes <- intersect(poids_fixes, colnames(table_post_sim))

  # Chercher les poids avec suffixe dynamique
  variables_dynamiques <- grep(
    paste0("^(", paste("poids", collapse = "|"), ")(|.*", suffixe, ")$"),
    colnames(table_post_sim),
    value = TRUE
  )
  
  # Combiner
  variables_ponderation <- c(poids_fixes, variables_dynamiques)
  
  # Exclusion finale
  variables_ponderation <- variables_ponderation[
    !(
      grepl("^poids_cnr_", variables_ponderation) |
        variables_ponderation == "poids_tirage"
    )
  ]
  
  # Chercher les colonnes des variables d'intérêt à partir de tous les préfixes
  variables_interet <- grep(paste0("^(", paste(prefix_var_interet, collapse = "|"), ")"), 
                            colnames(table_post_sim), value = TRUE)

  variables_sans_nr <- variables_ponderation[grepl("sans_nr", variables_ponderation)]
  
  # Préparer un data.table vide
  resultat <- data.table::data.table(
    variable_interet = character(),
    variable_ponderation = character(),
    somme_ponderee = numeric()
  )

  # Pour chaque combinaison de variable d'intérêt et de pondération
  for (var_int in variables_interet) {
    for (var_pond in variables_ponderation) {

      # Gérer le cas "recensement" (somme brute des répondants)
      if (var_pond %in% c("recensement", "poids_tirage_multimode", "poids_tirage_monomode", variables_sans_nr)) {
        somme <- sum(table_post_sim[[var_int]] * table_post_sim[[var_pond]], na.rm = TRUE)

      } else {
        somme <- sum(
          table_post_sim[[var_int]][table_post_sim[[nom_var_rep]] == 1] *
            table_post_sim[[var_pond]][table_post_sim[[nom_var_rep]] == 1], 
          na.rm = TRUE
        )
      }
      
      # Ajouter le résultat
      resultat <- rbind(resultat, data.table::data.table(
        variable_interet = var_int,
        variable_ponderation = var_pond,
        somme_ponderee = somme
      ))
    }
  }
  
  # Harmoniser les noms de certaines pondérations
  resultat <- resultat %>% 
    dplyr::mutate(variable_ponderation = dplyr::case_when(
      variable_ponderation == "poids_tirage_monomode" ~ "poids_tirage_mono_sans_nr",
      variable_ponderation == "poids_tirage_multimode" ~ "poids_tirage_multi_sans_nr",
      TRUE ~ variable_ponderation
    ))
  
  return(resultat)
}
