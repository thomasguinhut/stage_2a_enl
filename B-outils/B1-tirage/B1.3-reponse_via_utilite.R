source("A-donnees/A2-preparation_donnees/A2.2-rlogistic_gaussian_copula.R")

#' Simule les réponses multimode via utilité et copule logistique gaussienne
#'
#' @description
#' Cette fonction simule les réponses à partir des utilités données,
#' en utilisant une copule logistique gaussienne pour générer un bruit
#' corrélé selon la matrice sigma. 
#' La réponse téléphonique est conditionnée à la non-réponse internet.
#'
#' @param donnees [data.table] Données contenant les utilités des modes.
#' @param nom_colonne_utilite [character] Nom des colonnes d’utilité à utiliser dans `donnees`.
#'        Cette variable doit être un vecteur de deux noms correspondant aux colonnes d’utilité.
#' @param sigma [matrix] Matrice de covariance utilisée pour la copule logistique gaussienne.
#'
#' @return Une matrice logique (TRUE/FALSE) indiquant l’appartenance à la réponse pour chaque mode.
#'         La première colonne correspond à la réponse internet,
#'         la deuxième colonne à la réponse téléphone conditionnée.
#'
#' @details
#' - La fonction utilise la fonction `rlogistic_gaussian_copula` pour simuler les erreurs corrélées.
#' - La réponse au téléphone est ajustée pour ne se produire que si la réponse internet est absente.
#'
#' @examples
#' \dontrun{
#'   res_multimode <- reponse_multimode_via_utilite(donnees, c("utilite_int_1", "utilite_tel_1"), sigma)
#' }
#'
#' @export
reponse_multimode_via_utilite <- function(donnees, nom_colonne_utilite, sigma) {
  # Extraction des colonnes d'utilité sous forme de data.table
  utilites <- donnees[, ..nom_colonne_utilite]
  
  # Simulation des réalisations de la copule logistique gaussienne
  simu_logistique <- rlogistic_gaussian_copula(nrow(utilites), sigma = sigma, torch = FALSE)
  
  # Transposition pour aligner les dimensions (individus en lignes)
  simu_logistique <- t(simu_logistique)
  
  # Calcul de l'indicatrice de réponse (TRUE si utilité + bruit > 0)
  indicatrice_rep <- (utilites + simu_logistique) > 0
  
  # On impose que la réponse téléphone est conditionnée à la non-réponse internet
  indicatrice_rep[, 2] <- (1 - indicatrice_rep[, 1]) * indicatrice_rep[, 2]
  
  # On retourne la matrice logique
  return(indicatrice_rep)
}


#' Simule les réponses monomode via utilité et copule logistique gaussienne
#'
#' @description
#' Cette fonction simule les réponses monomodes en utilisant la copule logistique gaussienne
#' pour générer des erreurs corrélées à partir des utilités fournies.
#' Ici, la réponse téléphonique est toujours nulle.
#'
#' @param donnees [data.table] Données contenant les utilités des modes.
#' @param nom_colonne_utilite [character] Nom des colonnes d’utilité à utiliser dans `donnees`.
#'        Cette variable doit être un vecteur de deux noms correspondant aux colonnes d’utilité.
#' @param sigma [matrix] Matrice de covariance utilisée pour la copule logistique gaussienne.
#'
#' @return Une matrice logique indiquant la réponse uniquement pour le mode internet.
#'         La colonne téléphone est forcée à 0.
#'
#' @details
#' - La réponse téléphonique est toujours 0 dans ce cas (monomode).
#' - La fonction simule des erreurs via `rlogistic_gaussian_copula`.
#'
#' @examples
#' \dontrun{
#'   res_monomode <- reponse_monomode_via_utilite(donnees, c("utilite_int_1", "utilite_tel_1"), sigma)
#' }
#'
#' @export
reponse_monomode_via_utilite <- function(donnees, nom_colonne_utilite, sigma) {
  # Extraction des colonnes d'utilité
  utilites <- donnees[, ..nom_colonne_utilite]
  
  # Simulation des erreurs corrélées
  simu_logistique <- rlogistic_gaussian_copula(nrow(utilites), sigma = sigma, torch = FALSE)
  
  # Transposition pour aligner les lignes et colonnes
  simu_logistique <- t(simu_logistique)
  
  # Calcul indicatrice réponse (TRUE si utilité + bruit > 0)
  indicatrice_rep <- (utilites + simu_logistique) > 0
  
  # Réponse téléphone forcée à 0 dans le cas monomode
  indicatrice_rep[, 2] <- 0
  
  # Retourner la matrice logique des réponses
  return(indicatrice_rep)
}
