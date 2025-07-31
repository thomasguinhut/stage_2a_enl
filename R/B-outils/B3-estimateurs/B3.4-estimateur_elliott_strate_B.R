source("R/B-outils/B3-estimateurs/B3.4-estimateur_elliott.R")


estimateur_4B <- function(donnees, scenarios, variantes) {
  estimateur_elliott(donnees, scenarios, variantes, type = "B")
}
