#' Fonction générique pour faire des simulations, soit de façon parallèle,
#' soit de façon séquentielle.
#'
#' Cette fonction permet d'exécuter n'importe quelle fonction de traitement
#' plusieurs fois, avec ou sans parallélisation.
#'
#' Elle est utile par exemple pour des simulations, du bootstrap, 
#' des évaluations répétées de modèles, etc.
#'
#' @param nb_iterations Nombre d'itérations à exécuter (par exemple, 1000 répétitions)
#' @param fonction_traitement Fonction à exécuter à chaque itération.
#'   Cette fonction doit accepter au minimum les arguments : donnees, iteration, ...
#' @param donnees Jeu de données à traiter (data.frame, data.table, etc.)
#' @param parallel Logique (TRUE ou FALSE). TRUE = exécution parallèle. FALSE = séquentielle.
#' @param n_cores Nombre de cœurs processeur à utiliser (NULL = auto-détection)
#' @param batch_size Nombre d’itérations traitées ensemble (NULL = calcul automatique)
#' @param packages Vecteur des packages à charger sur chaque cœur (worker)
#' @param objets_env Liste d’objets à transmettre explicitement aux workers (ex : un modèle)
#'
#' @return Liste contenant les résultats de chaque itération.
#'
#' @export

traitement_parallele <- function(nb_iterations,
                                 fonction_traitement,
                                 donnees = NULL,
                                 parallel = TRUE,
                                 n_cores = NULL,
                                 batch_size = NULL,
                                 packages = c("dplyr", "data.table"),
                                 objets_env = list()
) {
  
  # ============================================================================
  # INITIALISATION
  # ============================================================================
  
  # On crée une liste vide pour stocker les résultats (1 par itération)
  resultats <- vector("list", nb_iterations)
  
  if (parallel) {
    
    # ------------------------------------------------------------------------
    # CONFIGURATION DU PARALLELISME
    # ------------------------------------------------------------------------
    
    # Si l'utilisateur n'a pas spécifié combien de cœurs utiliser, on détecte automatiquement.
    # On garde 1 cœur libre pour que le système (Windows/Linux/macOS) reste réactif.
    if (is.null(n_cores)) {
      n_cores <- min(parallel::detectCores() - 1, nb_iterations)
    }
    
    # On limite n_cores à des valeurs valides :
    # - pas plus que le nombre d'itérations
    # - pas plus que le nombre de cœurs disponibles
    n_cores <- max(1, min(n_cores, nb_iterations, parallel::detectCores() - 1))
    
    # Calcul automatique de la taille d’un batch :
    # Un batch est un petit groupe d’itérations qu’un cœur va traiter ensemble.
    # Cela évite que des cœurs restent inactifs pendant que d'autres travaillent encore.
    # Règle simple : on fait environ 3 batches par cœur pour répartir correctement.
    if (is.null(batch_size)) {
      batch_size <- max(1, ceiling(nb_iterations / (n_cores * 3)))
    }
    
    # On divise les itérations en batches (groupes d’indices)
    indices <- seq_len(nb_iterations)
    batches <- split(indices, ceiling(indices / batch_size))
    
    # ------------------------------------------------------------------------
    # GESTION DE LA MÉMOIRE
    # ------------------------------------------------------------------------
    
    # Pour éviter de copier plusieurs fois des objets volumineux (comme "donnees"),
    # on les sauvegarde une fois sur disque, et chaque worker les rechargera.
    temp_files <- list()
    
    # Sauvegarde temporaire des données principales
    if (!is.null(donnees)) {
      temp_files$donnees <- tempfile(fileext = ".rds")  # Fichier temporaire
      saveRDS(donnees, temp_files$donnees)  # Sauvegarde sur disque
      rm(donnees)  # Suppression de l’objet en mémoire pour libérer de la RAM
      gc()         # Appel au "garbage collector" = nettoyage mémoire
    }
    
    # Sauvegarde des objets supplémentaires fournis par l'utilisateur (par ex. un modèle)
    for (nom in names(objets_env)) {
      temp_files[[nom]] <- tempfile(fileext = ".rds")
      saveRDS(objets_env[[nom]], temp_files[[nom]])
    }
    
    # ------------------------------------------------------------------------
    # CRÉATION DU CLUSTER
    # ------------------------------------------------------------------------
    
    # Un "cluster" est un groupe de workers (cœurs) qui vont travailler en parallèle
    cl <- parallel::makeCluster(n_cores)
    
    # On s’assure que les ressources sont bien libérées même en cas d’erreur
    on.exit({
      parallel::stopCluster(cl)  # On arrête le cluster proprement
      for (f in temp_files) {
        if (file.exists(f)) unlink(f)  # Suppression des fichiers temporaires
      }
    }, add = TRUE)
    
    # ------------------------------------------------------------------------
    # EXPORT DES VARIABLES AUX WORKERS
    # ------------------------------------------------------------------------
    
    # Liste des variables à transmettre aux workers (obligatoire pour qu’ils aient accès)
    vars_export <- c("fonction_traitement", "temp_files", "packages")
    
    # On exporte les objets vers tous les workers du cluster
    parallel::clusterExport(cl, vars_export, envir = environment())
    
    # ------------------------------------------------------------------------
    # INITIALISATION DE CHAQUE WORKER
    # ------------------------------------------------------------------------
    
    # Chaque worker recharge les packages et les objets nécessaires
    parallel::clusterEvalQ(cl, {
      # On charge les packages requis
      for (pkg in packages) {
        library(pkg, character.only = TRUE)
      }
      
      # On recharge les données depuis le fichier RDS
      if (!is.null(temp_files$donnees)) {
        donnees <- readRDS(temp_files$donnees)
      }
      
      # On recharge les objets supplémentaires (modèles, paramètres, etc.)
      for (nom in names(temp_files)) {
        if (nom != "donnees") {
          assign(nom, readRDS(temp_files[[nom]]))
        }
      }
    })
    
    # ------------------------------------------------------------------------
    # FONCTION DE TRAITEMENT D’UN BATCH
    # ------------------------------------------------------------------------
    
    # C’est cette fonction qui est exécutée par les workers pour traiter un batch
    traiter_batch <- function(indices_batch) {
      resultats_batch <- list()
      
      for (idx in indices_batch) {
        # Appel de la fonction utilisateur avec les données et l’indice de l’itération
        resultat <- fonction_traitement(donnees, iteration = idx, ...)
        resultats_batch[[as.character(idx)]] <- resultat
      }
      
      # On retourne à la fois les indices et les résultats associés
      list(
        indices = indices_batch,
        resultats = resultats_batch
      )
    }
    
    # On envoie la fonction de traitement aux workers
    parallel::clusterExport(cl, "traiter_batch", envir = environment())
    
    # ------------------------------------------------------------------------
    # EXÉCUTION PARALLÈLE
    # ------------------------------------------------------------------------
    
    # On utilise parLapplyLB (Load Balancing) pour répartir dynamiquement les batches :
    # Si un worker finit plus tôt, il prend un batch suivant → évite qu’il reste inactif
    resultats_batches <- parallel::parLapplyLB(cl, batches, traiter_batch)
    
    # ------------------------------------------------------------------------
    # RECONSTRUCTION DES RÉSULTATS
    # ------------------------------------------------------------------------
    
    # On assemble les résultats dans l’ordre d’origine
    for (i in seq_along(resultats_batches)) {
      batch_result <- resultats_batches[[i]]
      for (idx_str in names(batch_result$resultats)) {
        idx <- as.numeric(idx_str)
        resultats[[idx]] <- batch_result$resultats[[idx_str]]
      }
    }
    
  } else {
    
    # ========================================================================
    # MODE SÉQUENTIEL (SANS PARALLÈLE)
    # ========================================================================
    
    # Si l’utilisateur ne veut pas de parallélisme, on exécute simplement chaque itération
    for (i in seq_len(nb_iterations)) {
      resultats[[i]] <- fonction_traitement(donnees, iteration = i, ...)
    }
  }
  
  # ==========================================================================
  # RETOUR DES RÉSULTATS
  # ==========================================================================
  
  return(resultats)
}
