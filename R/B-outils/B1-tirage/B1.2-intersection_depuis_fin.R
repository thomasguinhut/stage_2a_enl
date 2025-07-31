#' Trouve l'intersection de fin entre deux chaînes de caractères
#'
#' @description
#' Cette fonction cherche la sous-chaîne commune la plus longue entre deux chaînes,
#' en commençant la comparaison par la fin de chaque chaîne.
#' 
#' Elle peut être utile pour détecter des suffixes communs entre deux identifiants, variables, noms de fichiers, etc.
#'
#' @param str1 [character(1)] Première chaîne de caractères à comparer.
#' @param str2 [character(1)] Deuxième chaîne de caractères à comparer.
#'
#' @return [character(1)] La sous-chaîne commune depuis la fin des deux chaînes, ou une chaîne vide si aucune intersection.
#'
#' @examples
#' intersection_depuis_fin("toto_ajtj_il", "tat_aljt_il")  # "_il"
#' intersection_depuis_fin("abc@123", "abc@1234")          # ""
#' intersection_depuis_fin("abcdef", "xyz123")             # ""
#'
#' @importFrom stringi stri_reverse
#' @export
intersection_depuis_fin <- function(str1, str2) {
  # Vérification de la validité des entrées
  if (!is.character(str1) || !is.character(str2)) {
    stop("Les deux arguments doivent être des chaînes de caractères.")
  }
  if (length(str1) != 1 || length(str2) != 1) {
    stop("Les deux arguments doivent être des chaînes de longueur 1.")
  }
  
  # Étape 1 : inverser les deux chaînes pour comparer depuis la fin
  str1_inverse <- stringi::stri_reverse(str1)
  str2_inverse <- stringi::stri_reverse(str2)
  
  # Étape 2 : déterminer la longueur maximale possible pour la comparaison
  max_len <- min(nchar(str1_inverse), nchar(str2_inverse))
  
  # Étape 3 : initialiser une chaîne vide pour stocker la partie commune
  commun_inverse <- ""
  
  # Étape 4 : comparer caractère par caractère en partant du début des chaînes inversées
  for (i in 1:max_len) {
    # Extraire le i-ème préfixe des chaînes inversées
    sous_str1 <- substr(str1_inverse, 1, i)
    sous_str2 <- substr(str2_inverse, 1, i)
    
    if (sous_str1 == sous_str2) {
      # Si les préfixes sont identiques, mettre à jour la chaîne commune
      commun_inverse <- sous_str1
    } else {
      # Sinon, on a atteint la fin de l'intersection
      break
    }
  }
  
  # Étape 5 : retourner l'inverse de la chaîne commune trouvée
  return(stringi::stri_reverse(commun_inverse))
}
