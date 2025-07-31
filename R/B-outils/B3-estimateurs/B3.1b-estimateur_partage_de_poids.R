source("R/B-outils/B3-estimateurs/B3.1-estimateurs_hartley.R")

estimateur_1b <- function(donnees,
                          scenarios = c("1", "2"),
                          variantes) {
  donnees$poids_1b_sans_nr <- 1 / 2 * donnees$poids_tirage

  for (i in scenarios) {
    donnees <- estimateur_hartley(donnees, i, variantes, 1 / 2, "1b")
  }

  return(donnees)
}
