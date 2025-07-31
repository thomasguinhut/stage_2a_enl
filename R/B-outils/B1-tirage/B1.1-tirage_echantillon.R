#' Tirage d'un échantillon stratifié à deux modes (multimode et monomode)
#'
#' @description
#' Effectue un tirage stratifié sur deux strates (A et B) pour deux modes de collecte
#' (multimode et monomode), en respectant des tailles et proportions spécifiées.
#'
#' @param donnees data.frame ou data.table représentant la population.
#' @param n_multi Taille de l'échantillon multimode (optionnel).
#' @param n_mono Taille de l'échantillon monomode (optionnel).
#' @param part_strate_A_dans_mode_1 Proportion de la strate A dans l’échantillon multimode.
#' @param part_strate_A_dans_mode_2 Proportion de la strate A dans l’échantillon monomode.
#' @param nom_var_prob_internet Nom de la variable pour la proba d’accès Internet (défaut : "pi_int").
#' @param nom_var_prob_tel Nom de la variable pour la proba d’accès téléphone (défaut : "pi_tel").
#' @param strat_var_name Nom de la variable identifiant les strates (défaut : "strate_vec").
#'
#' @return Un data.frame avec indicateurs et poids de sondage pour chaque mode et globalement.
#'
#' @importFrom sampling strata srswor
#' @importFrom data.table as.data.table
#'
#' @export
tirage_echantillon <- function(donnees,
                               n_multi = NULL,
                               n_mono = NULL,
                               part_strate_A_dans_mode_1,
                               part_strate_A_dans_mode_2,
                               nom_var_prob_internet = "pi_int",
                               nom_var_prob_tel = "pi_tel",
                               strat_var_name = "strate_vec") {
  
  # Conversion en data.table
  donnees <- data.table::as.data.table(donnees)
  
  # Effectifs des strates
  N_A <- sum(donnees[[strat_var_name]] == "A")
  N_B <- sum(donnees[[strat_var_name]] == "B")
  
  # Calcul automatique des tailles si non fournies
  if (is.null(n_multi) || is.null(n_mono)) {
    warning("n_multi ou n_mono non fourni : calcul automatique.")
    n_multi <- round((N_B * (1 - part_strate_A_dans_mode_2) - part_strate_A_dans_mode_2 * N_B) /
                       (part_strate_A_dans_mode_1 - part_strate_A_dans_mode_2))
    n_mono <- nrow(donnees) - n_multi
    if (n_multi <= 0 || n_mono <= 0) {
      stop("Proportions invalides : tailles d’échantillons négatives.")
    }
  }
  
  # Calcul du nombre d’unités par strate et mode
  n_A_mode_1 <- round(part_strate_A_dans_mode_1 * n_multi)
  n_B_mode_1 <- n_multi - n_A_mode_1
  n_A_mode_2 <- round(part_strate_A_dans_mode_2 * n_mono)
  n_B_mode_2 <- n_mono - n_A_mode_2
  n_A_total <- n_A_mode_1 + n_A_mode_2
  n_B_total <- n_B_mode_1 + n_B_mode_2
  
  # Correction si les tailles dépassent les effectifs
  if (n_A_total > N_A) {
    surplus <- n_A_total - N_A
    n_A_mode_1 <- n_A_mode_1 - round(surplus / 2)
    n_A_mode_2 <- n_A_mode_2 - (surplus - round(surplus / 2))
  }
  if (n_B_total > N_B) {
    surplus <- n_B_total - N_B
    n_B_mode_1 <- n_B_mode_1 - round(surplus / 2)
    n_B_mode_2 <- n_B_mode_2 - (surplus - round(surplus / 2))
  }
  
  # Tirage aléatoire sans remise (stratifié)
  echantillon <- sampling::strata(
    data = donnees,
    stratanames = strat_var_name,
    size = c(n_A_mode_1 + n_A_mode_2, n_B_mode_1 + n_B_mode_2),
    method = "srswor"
  )
  
  ids_tires <- echantillon$ID_unit
  donnees$ind_tirage <- 0
  donnees$ind_tirage[ids_tires] <- 1
  donnees$ind_tirage_multimode <- 0
  donnees$ind_tirage_monomode <- 0
  
  # Répartition entre modes pour les tirés de chaque strate
  lignes_A <- which(donnees[[strat_var_name]] == "A" & donnees$ind_tirage == 1)
  donnees$ind_tirage_multimode[lignes_A[sampling::srswor(n_A_mode_1, length(lignes_A)) == 1]] <- 1
  
  lignes_B <- which(donnees[[strat_var_name]] == "B" & donnees$ind_tirage == 1)
  donnees$ind_tirage_multimode[lignes_B[sampling::srswor(n_B_mode_1, length(lignes_B)) == 1]] <- 1
  
  donnees$ind_tirage_monomode <- ifelse(
    donnees$ind_tirage == 1 & donnees$ind_tirage_multimode == 0, 1, 0
  )
  
  # Vérification d'exclusivité des échantillons
  if (any(donnees$ind_tirage_multimode & donnees$ind_tirage_monomode)) {
    stop("Erreur : certaines unités appartiennent aux deux échantillons.")
  }
  
  # Initialisation des poids
  donnees[, c("poids_tirage_ech_multimode", "poids_tirage_ech_monomode", "poids_tirage_multimode",
              "poids_tirage_monomode", "poids_tirage") := 0]
  
  # Attribution des poids - multimode
  donnees$poids_tirage_ech_multimode[donnees[[strat_var_name]] == "A"] <- N_A / n_A_mode_1
  donnees$poids_tirage_ech_multimode[donnees[[strat_var_name]] == "B"] <- N_B / n_B_mode_1
  donnees$poids_tirage_multimode[donnees$ind_tirage_multimode == 1 &
                                       donnees[[strat_var_name]] == "A"] <- N_A / n_A_mode_1
  donnees$poids_tirage_multimode[donnees$ind_tirage_multimode == 1 &
                                       donnees[[strat_var_name]] == "B"] <- N_B / n_B_mode_1
  
  # Attribution des poids - monomode
  donnees$poids_tirage_ech_monomode[donnees[[strat_var_name]] == "A"] <- N_A / n_A_mode_2
  donnees$poids_tirage_ech_monomode[donnees[[strat_var_name]] == "B"] <- N_B / n_B_mode_2
  donnees$poids_tirage_monomode[donnees$ind_tirage_monomode == 1 &
                                      donnees[[strat_var_name]] == "A"] <- N_A / n_A_mode_2
  donnees$poids_tirage_monomode[donnees$ind_tirage_monomode == 1 &
                                      donnees[[strat_var_name]] == "B"] <- N_B / n_B_mode_2
  
  # Poids globaux
  donnees$poids_tirage <- donnees$poids_tirage_multimode + donnees$poids_tirage_monomode
  
  # Probabilités de tirage
  donnees$proba_tirage_ech_multimode <- ifelse(donnees$poids_tirage_ech_multimode > 0,
                                           1 / donnees$poids_tirage_ech_multimode, 0)
  donnees$proba_tirage_ech_monomode <- ifelse(donnees$poids_tirage_ech_monomode > 0,
                                          1 / donnees$poids_tirage_ech_monomode, 0)
  
  return(donnees)
}
