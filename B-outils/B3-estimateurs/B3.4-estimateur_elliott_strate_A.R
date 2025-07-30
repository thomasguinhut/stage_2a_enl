source("B-outils/B3-estimateurs/B3.4-estimateur_elliott.R")

estimateur_4A <- function(donnees, scenarios, variantes) {
  estimateur_elliott(donnees, scenarios, variantes, type = "A")
}
