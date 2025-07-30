source("B-outils/B1-tirage/B1.1-tirage_echantillon.R")
source("B-outils/B1-tirage/B1.4-tirage_repondants.R")

#' Tirage d'un échantillon stratifié en deux modes avec simulation des répondants
#'
#' @description
#' Cette fonction réalise un tirage d'échantillon stratifié à deux modes (multimode et monomode),
#' puis simule les répondants selon les probabilités ou modèles latents fournis.
#' Elle encapsule les appels aux fonctions `tirage_echantillon` et `tirage_repondants` (ou `tirage_repondants_latent`).
#'
#' @param donnees Data.frame ou data.table contenant les données populationnelles.
#' @param n_multi Entier ou NULL, taille de l'échantillon multimode. Si NULL, calculée automatiquement.
#' @param n_mono Entier ou NULL, taille de l'échantillon monomode. Si NULL, calculée automatiquement.
#' @param part_strate_A_dans_mode_1 Numérique entre 0 et 1, proportion de la strate A dans l'échantillon multimode.
#' @param part_strate_A_dans_mode_2 Numérique entre 0 et 1, proportion de la strate A dans l'échantillon monomode.
#' @param nom_var_prob_internet Caractère ou vecteur de caractères, noms des variables de probabilité réponse internet.
#'                             Par défaut NULL si modèle latent utilisé.
#' @param nom_var_prob_tel Caractère ou vecteur de caractères, noms des variables de probabilité réponse téléphone.
#'                         Par défaut NULL si modèle latent utilisé.
#' @param nom_var_utilite_internet Caractère ou vecteur de caractères, noms des variables d'utilité internet (pour modèle latent).
#'                                Par défaut NULL.
#' @param nom_var_utilite_tel Caractère ou vecteur de caractères, noms des variables d'utilité téléphone (pour modèle latent).
#'                            Par défaut NULL.
#' @param sigma Matrice ou NULL, matrice de covariance pour modèle latent.
#' @param strat_var_name Caractère, nom de la variable de stratification dans les données (par défaut "strate_vec").
#' @param modele_latent Booléen, indique si la simulation des répondants utilise un modèle latent (TRUE) ou des probabilités (FALSE).
#'
#' @return
#' Un data.table enrichi des variables suivantes :
#' \itemize{
#'   \item indicatrices d'appartenance aux échantillons (multimode, monomode),
#'   \item poids de sondage,
#'   \item indicatrices de réponse internet et téléphone,
#'   \item indicatrices de répondants dans chaque mode et globales,
#'   \item suffixées si plusieurs simulations sont effectuées.
#' }
#'
#' @details
#' - Convertit les données en data.table si nécessaire.
#' - Tire un échantillon stratifié à deux modes via `tirage_echantillon`.
#' - Pour chaque scénario de probabilités ou utilités, simule les répondants avec
#'   `tirage_repondants` ou `tirage_repondants_latent` en ajoutant un suffixe si plusieurs scénarios.
#'
#' @examples
#' \dontrun{
#' donnees <- data.frame(
#'   id = 1:1000,
#'   strate_vec = sample(c("A", "B"), 1000, replace = TRUE),
#'   pi_internet = runif(1000),
#'   pi_tel = runif(1000)
#' )
#' result <- tirage(
#'   donnees,
#'   n_multi = 200,
#'   n_mono = 300,
#'   part_strate_A_dans_mode_1 = 0.3,
#'   part_strate_A_dans_mode_2 = 0.5,
#'   nom_var_prob_internet = "pi_internet",
#'   nom_var_prob_tel = "pi_tel",
#'   modele_latent = FALSE
#' )
#' }
#'
#' @importFrom data.table as.data.table setDT
#' @export
tirage <- function(donnees, 
                   n_multi,
                   n_mono,
                   part_strate_A_dans_mode_1,
                   part_strate_A_dans_mode_2,
                   nom_var_prob_internet = NULL,
                   nom_var_prob_tel = NULL,
                   scenarios, 
                   sigma = NULL,
                   strat_var_name,
                   modele_latent = TRUE) {
  
  # Contrôle des arguments selon le type de modèle (latent ou pas)
  if (modele_latent) {
    if (is.null(scenarios) || is.null(sigma)) {
      stop("Pour modele_latent=TRUE, scenarios et sigma ne doivent pas être NULL.")
    }
  } else {
    if (is.null(nom_var_prob_internet) || is.null(nom_var_prob_tel)) {
      stop("Pour modele_latent=FALSE, nom_var_prob_internet et nom_var_prob_tel ne doivent pas être NULL.")
    }
  }
  
  # Nombre de scénarios selon le type de données fournies
  if (modele_latent) {
    nb_scenario <- length(scenarios)
  } else {
    nb_scenario <- length(nom_var_prob_internet)
  }
  
  # Conversion en data.table si ce n'est pas déjà le cas
  if (!data.table::is.data.table(donnees)) {
    donnees <- data.table::as.data.table(donnees)
  }
  
  # Tirage de l'échantillon stratifié à deux modes
  donnees <- tirage_echantillon(
    donnees,
    n_multi = n_multi,
    n_mono = n_mono,
    part_strate_A_dans_mode_1 = part_strate_A_dans_mode_1,
    part_strate_A_dans_mode_2 = part_strate_A_dans_mode_2,
    strat_var_name = strat_var_name
  )
  
  # Boucle sur chaque scénario pour simuler les répondants
  for (i in seq_len(nb_scenario)) {
    # Définition du suffixe : vide si un seul scénario, sinon _1, _2, ...
    suffix <- if (nb_scenario == 1) NULL else paste0("_", i)
    # Appel à la bonne fonction selon le modèle
    if (modele_latent) {
      donnees <- tirage_repondants(
        donnees,
        sigma = sigma,
        latent = TRUE,
        scenarios[i],
        suffix = suffix
      )
    } else {
      donnees <- tirage_repondants(
        donnees,
        latent = FALSE,
        nom_var_prob_internet = nom_var_prob_internet[i],
        nom_var_prob_tel = nom_var_prob_tel[i],
        suffix = suffix
      )
    }
  }
  
  # Retourner les données enrichies
  return(donnees)
}
