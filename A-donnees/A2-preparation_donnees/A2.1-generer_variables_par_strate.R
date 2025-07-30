#' Génère des variables par strate
#'
#' Pour chaque colonne dont le nom commence par un préfixe donné, la fonction
#' crée deux nouvelles colonnes :
#' - une contenant les valeurs si `strate_vec == "A"` (et 0 sinon)
#' - une contenant les valeurs si `strate_vec == "B"` (et 0 sinon)
#'
#' @param donnees Un `data.frame` ou `data.table` contenant les données.
#'        Doit inclure une colonne nommée `strate_vec` contenant les valeurs `"A"` ou `"B"`.
#' @param prefixes Un vecteur de chaînes de caractères correspondant aux préfixes
#'        des noms de colonnes à dupliquer par strate (ex : `c("z", "x")`).
#'
#' @return Un objet `data.table` avec les colonnes d'origine augmentées de nouvelles colonnes
#'         suffixées par `_strate_A` et `_strate_B`, contenant les valeurs conditionnelles.
#'
#' @examples
#' dt <- data.table(strate_vec = c("A", "B"), z1 = c(10, 20))
#' generer_variables_par_strate(dt, prefixes = c("z"))
#'
#' @export
generer_variables_par_strate <- function(donnees, prefixes) {
  # Vérifier que donnees est une data.table
  if (!is.data.table(donnees)) {
    donnees <- as.data.table(donnees)
  }
  
  # Sélectionner les colonnes dont le nom commence par l’un des préfixes
  # Exemple : prefixes = c("z", "x") sélectionne z1, x_total, etc.
  cols <- grep(paste0("^", paste(prefixes, collapse = "|")), names(donnees), value = TRUE)
  
  # Créer deux nouvelles colonnes pour chaque variable sélectionnée :
  # - une pour les individus de strate A
  # - une pour les individus de strate B
  for (col in cols) {
    # Colonne contenant la valeur si strate_vec == "A", sinon 0
    donnees[[paste0(col, "_strate_A")]] <- ifelse(donnees$strate_vec == "A", donnees[[col]], 0)
    
    # Colonne contenant la valeur si strate_vec == "B", sinon 0
    donnees[[paste0(col, "_strate_B")]] <- ifelse(donnees$strate_vec == "B", donnees[[col]], 0)
  }
  
  # Retourner l'objet data.table enrichi
  return(donnees)
}
