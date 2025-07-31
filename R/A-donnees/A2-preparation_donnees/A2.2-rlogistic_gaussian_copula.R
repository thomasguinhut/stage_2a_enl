#' Génère des échantillons bivariés logistiques corrélés via une copule gaussienne
#'
#' Cette fonction simule `n` paires de variables aléatoires suivant une
#' **copule gaussienne** avec des **marges logistiques**.
#' La dépendance entre les deux variables est définie par la matrice de covariance `sigma`.
#' L'implémentation peut utiliser `torch` ou les fonctions standards de R.
#' Fonction utilisée pour générer les probabilités de réponse selon les utilités par mode
#' dans B1.3-reponse_via_utilite.R.
#'
#' @param n Integer. Nombre d'échantillons à générer.
#' @param sigma Matrice de covariance 2x2 définissant la dépendance entre les deux variables.
#' @param mu1 Numeric. Moyenne de la première marge logistique (par défaut 0).
#' @param mu2 Numeric. Moyenne de la deuxième marge logistique (par défaut 0).
#' @param torch Logical. Si TRUE, utilise `torch` pour l'accélération GPU (par défaut TRUE).
#'
#' @return
#' - Si `torch = TRUE` : retourne un tenseur `torch_tensor` de dimension `(n, 2)`.
#' - Si `torch = FALSE` : retourne une matrice `2 × n`.
#'
#' @examples
#' sigma <- matrix(c(1, 0.8, 0.8, 1), 2, 2)
#' data <- rlogistic_gaussian_copula(1000, sigma, torch = FALSE)
#' plot(data[1,], data[2,], pch = 16, col = rgb(0,0,1,0.3))
rlogistic_gaussian_copula <- function(n, sigma, mu1 = 0, mu2 = 0, torch = TRUE) {
  if (torch) {
    # --- Implémentation torch ---
    
    # Décomposition de Cholesky pour obtenir la matrice L telle que sigma = L %*% t(L)
    L <- torch_cholesky(sigma)
    
    # Générer un vecteur de normales standard bivariées corrélées via L
    Z <- torch_matmul(L, torch_randn(c(2, n)))  # Z est de taille (2, n)
    
    # Transformation en variables uniformes via la CDF normale (approximation par erf)
    # U ~ Uniform(0,1) grâce à la transformation de la normale
    U <- torch_erf(Z / sqrt(2)) * 0.5 + 0.5
    
    # Transformation en loi logistique via l'inverse de la CDF logistique
    X1 <- mu1 + torch_log(U[1, ] / (1 - U[1, ]))
    X2 <- mu2 + torch_log(U[2, ] / (1 - U[2, ]))
    
    # Empilement en un tenseur de taille (n, 2)
    return(torch_stack(list(X1, X2), dim = 1))
  } else {
    # --- Implémentation standard R ---
    
    # Si sigma est un objet torch, le convertir en matrice R
    if (inherits(sigma, "torch_tensor")) {
      sigma <- as.matrix(sigma)
    }
    
    # Décomposition de Cholesky
    L <- chol(sigma)
    
    # Générer des normales standard et corrélées : Z ~ N(0, sigma)
    Z <- t(L) %*% matrix(rnorm(2 * n), nrow = 2, ncol = n)
    
    # Transformation normale -> uniforme
    U <- pnorm(Z)
    
    # Transformation uniforme -> logistique
    X1 <- mu1 + log(U[1, ] / (1 - U[1, ]))
    X2 <- mu2 + log(U[2, ] / (1 - U[2, ]))
    
    # Retourner une matrice 2 lignes (X1, X2) × n colonnes
    return(rbind(X1, X2))
  }
}
