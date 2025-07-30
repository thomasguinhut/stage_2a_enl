ghr_quantile <- function(proba_estimees,
                         ind_rep,
                         poids_tirage,
                         grh,
                         taux_min_grh,
                         poids_tirage_en_amont = NULL,
                         ind_rep_en_amont = NULL,
                         proba_estimees_en_amont = NULL) {
  
  # Si aucun poids fourni, on utilise des poids unitaires
  if (is.null(poids_tirage)) {
    poids_tirage <- rep(1, length(proba_estimees))
  }
  
  # Si pas de données en amont, on travaille uniquement avec les données actuelles
  en_amont_manquant <- is.null(proba_estimees_en_amont)
  
  if (en_amont_manquant) {
    proba_estimees_en_amont <- proba_estimees
    poids_tirage_en_amont <- poids_tirage
    ind_rep_en_amont <- ind_rep
  } else {
    # Si on donne des données "en amont", on complète les poids si besoin
    if (is.null(poids_tirage_en_amont)) {
      poids_tirage_en_amont <- rep(1, length(proba_estimees_en_amont))
    }
  }
  
  # Vérification que les vecteurs en amont sont de même taille
  if (length(proba_estimees_en_amont) != length(ind_rep_en_amont)) {
    stop("Les longueurs de 'proba_estimees_en_amont' et 'ind_rep_en_amont' doivent être identiques.")
  }
  
  # Vérifie que ind_rep_en_amont contient bien que des 0 ou 1
  if (!all(ind_rep_en_amont %in% c(0, 1))) {
    stop("'ind_rep_en_amont' doit contenir uniquement des 0 ou 1.")
  }
  
  # Calcul des bornes pour les quantiles, en ajoutant -Inf et +Inf pour couvrir tout
  quantiles <- quantile(proba_estimees_en_amont,
                        probs = seq(0, 1, length.out = grh + 1),
                        na.rm = TRUE)
  bornes <- unique(c(-Inf, quantiles[-c(1, length(quantiles))], Inf))
  
  # Attribution de chaque individu à un groupe (en amont et principal)
  groupe_amont <- cut(proba_estimees_en_amont, breaks = bornes, include.lowest = TRUE)
  groupe_actuel <- cut(proba_estimees, breaks = bornes, include.lowest = TRUE)
  
  # Calcul des réponses pondérées par groupe
  numerateur <- tapply(poids_tirage_en_amont * ind_rep_en_amont, groupe_amont, sum)
  denominateur <- tapply(poids_tirage_en_amont, groupe_amont, sum)
  
  proba_moyenne_par_groupe <- numerateur / denominateur
  
  # Appliquer le plancher taux_min_grh si spécifié
  if (!is.null(taux_min_grh)) {
    proba_moyenne_par_groupe[proba_moyenne_par_groupe < taux_min_grh] <- taux_min_grh
  }
  
  # Création du vecteur de sortie : on remplace chaque proba par la moyenne de son groupe
  proba_corrigee <- proba_moyenne_par_groupe[as.character(groupe_actuel)]
  
  # Résultat à retourner
  resultat <- list()
  resultat$proba_estimees <- proba_corrigee
  
  # Si jeu en amont fourni, on ajoute les moyennes "en amont" aussi
  if (!en_amont_manquant) {
    proba_en_amont_corrigee <- proba_moyenne_par_groupe[as.character(groupe_amont)]
    resultat$proba_estimees_en_amont <- proba_en_amont_corrigee
  }
  
  resultat$grh_groupe <- as.integer(groupe_actuel)  # conversion en numéro de groupe
  
  return(resultat)
}
