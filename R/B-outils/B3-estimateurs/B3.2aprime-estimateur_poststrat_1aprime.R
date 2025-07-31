source("R/B-outils/B3-estimateurs/B3.1-estimateurs_hartley.R")

estimateur_2aprime <- function(donnees, scenarios = c("1", "2"), variantes) {
  # Copier les données pour éviter modification par référence
  resultat <- copy(donnees)

  # Initialiser la colonne des poids sans NR
  resultat$poids_2aprime_sans_nr <- NA_real_

  # Traiter chaque strate séparément
  for (strate in unique(donnees$strate_vec)) {
    # Sous-échantillon de la strate
    ind_strate <- donnees$strate_vec == strate
    donnees_strate <- donnees[ind_strate]

    for (i in scenarios) {
      # Nombre total de répondants dans le monomode (pour cette strate et ce scénario)
      nb_rep_mono <- sum(donnees_strate[[paste0("rep_mono_", i)]], na.rm = TRUE)

      # Nombre total de répondants dans l'ensemble (mono + multi)
      nb_rep_total <- sum(donnees_strate[[paste0("rep_", i)]], na.rm = TRUE)

      # Proportion de répondants issus du monomode
      prop_rep_mono <- nb_rep_mono / nb_rep_total

      # Coefficient selon l'appartenance à un sous-échantillon
      coef <- prop_rep_mono * donnees_strate$ind_tirage_monomode +
        (1 - prop_rep_mono) * donnees_strate$ind_tirage_multimode

      # Poids sans non-réponse pour ce scénario
      # On écrase à chaque passage mais c'est voulu car poids_2aprime_sans_nr global, pas spécifique au scénario
      resultat$poids_2aprime_sans_nr[ind_strate] <- coef * donnees_strate$poids_tirage

      # Appliquer Hartley pour ce scénario sur la strate
      donnees_strate <- estimateur_hartley(
        donnees_strate, i, variantes,
        coef * donnees_strate[[paste0("rep_", i)]],
        "2aprime"
      )
    }

    # Récupérer les colonnes nouvelles créées par Hartley et les insérer dans résultat
    nouvelles_cols <- setdiff(names(donnees_strate), names(donnees))
    for (col in nouvelles_cols) {
      if (!col %in% names(resultat)) {
        resultat[[col]] <- NA_real_
      }
      resultat[[col]][ind_strate] <- donnees_strate[[col]]
    }
  }

  return(resultat)
}
