extract_exact_coefficients <- function(data) {
  # Initialisation des résultats
  results <- list()
  
  # Sélection des variables X, Y, Z (inclure x_4 et x_5)
  x_vars <- grep("^x_[0-9]+$", names(data), value = TRUE)
  y_vars <- grep("^y_[0-9]+_total$", names(data), value = TRUE)
  z_vars <- grep("^z_[0-9]+$", names(data), value = TRUE)
  
  # Vérification et conversion des colonnes en numérique
  for (var in c(x_vars, y_vars, z_vars)) {
    data[[var]] <- as.numeric(data[[var]])
  }
  
  # Boucle sur les 4 scénarios
  for (s in 1:4) {
    # Noms des colonnes d'utilité pour le scénario s
    utilite_int_col <- paste0("utilite_int_", s)
    utilite_tel_col <- paste0("utilite_tel_", s)
    
    # Vérification et conversion des colonnes d'utilité en numérique
    data[[utilite_int_col]] <- as.numeric(data[[utilite_int_col]])
    data[[utilite_tel_col]] <- as.numeric(data[[utilite_tel_col]])
    
    # Suppression des lignes avec des NA
    cols_to_check <- c(x_vars, y_vars, z_vars, utilite_int_col, utilite_tel_col)
    cols_to_check <- cols_to_check[cols_to_check %in% names(data)]
    complete_cases_data <- na.omit(data[, cols_to_check, with = FALSE])
    
    # Matrice des variables explicatives (sans l'intercept)
    X <- as.matrix(complete_cases_data[, .SD, .SDcols = c(x_vars, y_vars, z_vars)])
    
    # Vecteurs des utilités
    U_int <- complete_cases_data[[utilite_int_col]]
    U_tel <- complete_cases_data[[utilite_tel_col]]
    
    # Estimation des coefficients par résolution directe
    coef_int <- solve(t(X) %*% X, t(X) %*% U_int)
    coef_tel <- solve(t(X) %*% X, t(X) %*% U_tel)
    
    # Noms des coefficients
    names(coef_int) <- c(x_vars, y_vars, z_vars)
    names(coef_tel) <- c(x_vars, y_vars, z_vars)
    
    # Forcer les paramètres delta et gamma à 0 pour le scénario 1
    if (s == 1) {
      coef_int[names(coef_int) %in% z_vars] <- 0  # Deltas à 0
      coef_int[names(coef_int) %in% y_vars] <- 0  # Gammas à 0
      coef_tel[names(coef_tel) %in% z_vars] <- 0  # Deltas à 0
      coef_tel[names(coef_tel) %in% y_vars] <- 0  # Gammas à 0
    }
    
    # Stockage des résultats
    results[[paste0("scenario_", s)]] <- list(
      internet = coef_int,
      telephone = coef_tel
    )
  }
  
  return(results)
}

# Exécuter la fonction
coefs_exacts <- extract_exact_coefficients(bdd)

# Affichage des résultats sous forme de tableau
for (s in 1:4) {
  cat("\n--- Scénario", s, "---\n")
  cat("Coefficients pour l'utilité internet :\n")
  print(coefs_exacts[[paste0("scenario_", s)]]$internet)
  cat("\nCoefficients pour l'utilité téléphone :\n")
  print(coefs_exacts[[paste0("scenario_", s)]]$telephone)
}

# Création d'un tableau récapitulatif avec Beta4 et Beta5
tableau_coefficients <- data.frame(
  Scénario = integer(),
  Mode = character(),
  Beta1 = numeric(),
  Beta2 = numeric(),
  Beta3 = numeric(),
  Beta4 = numeric(),
  Beta5 = numeric(),
  Gamma1 = numeric(),
  Gamma2 = numeric(),
  Gamma3 = numeric(),
  Delta1 = numeric(),
  Delta2 = numeric(),
  Delta3 = numeric()
)

for (s in 1:4) {
  # Internet
  coef_int <- coefs_exacts[[paste0("scenario_", s)]]$internet
  tableau_coefficients <- rbind(tableau_coefficients,
                                data.frame(
                                  Scénario = s,
                                  Mode = "Internet",
                                  Beta1 = coef_int["x_1"],
                                  Beta2 = coef_int["x_2"],
                                  Beta3 = coef_int["x_3"],
                                  Beta4 = coef_int["x_4"],
                                  Beta5 = coef_int["x_5"],
                                  Gamma1 = coef_int["y_1_total"],
                                  Gamma2 = coef_int["y_2_total"],
                                  Gamma3 = coef_int["y_3_total"],
                                  Delta1 = coef_int["z_1"],
                                  Delta2 = coef_int["z_2"],
                                  Delta3 = coef_int["z_3"]
                                ))
  
  # Téléphone
  coef_tel <- coefs_exacts[[paste0("scenario_", s)]]$telephone
  tableau_coefficients <- rbind(tableau_coefficients,
                                data.frame(
                                  Scénario = s,
                                  Mode = "Téléphone",
                                  Beta1 = coef_tel["x_1"],
                                  Beta2 = coef_tel["x_2"],
                                  Beta3 = coef_tel["x_3"],
                                  Beta4 = coef_tel["x_4"],
                                  Beta5 = coef_tel["x_5"],
                                  Gamma1 = coef_tel["y_1_total"],
                                  Gamma2 = coef_tel["y_2_total"],
                                  Gamma3 = coef_tel["y_3_total"],
                                  Delta1 = coef_tel["z_1"],
                                  Delta2 = coef_tel["z_2"],
                                  Delta3 = coef_tel["z_3"]
                                ))
}

# Affichage du tableau récapitulatif
print(tableau_coefficients)

table(bdd)
