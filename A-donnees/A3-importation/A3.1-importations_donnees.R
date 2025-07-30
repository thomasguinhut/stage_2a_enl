source("A-donnees/A2-preparation_donnees/A2.1-generer_variables_par_strate.R")

if (!(exists("sigma_1"))) {
  sigma_1 <- matrix(0.5, 2, 2)
  diag(sigma_1) <- 1 
}

if (!(exists("bdd"))) {
  
  pacman::p_load(dplyr, data.table)
  
  load("A-donnees/donnees_brut.RData")
  bdd_1 <- data.frame(obj[[1]])

  bdd_2 <- bdd_1 %>% 
    select(!starts_with("z"))
  
  bdd_3 <- generer_variables_par_strate(bdd_2, c("y_", "x_"))
  
  bdd_4 <- bdd_3 %>% 
    mutate(recensement = 1)
  
  bdd_5 <- bdd_4 %>% 
    rename(y_1_total = y_1,
           y_2_total = y_2,
           y_3_total = y_3,
           y_4_total = y_4,
           y_5_total = y_5,
           y_6_total = y_6,
           y_7_total = y_7,
           y_8_total = y_8,
           y_9_total = y_9,
           y_10_total = y_10)

  bdd <- bdd_5
  
  rm(bdd_1, bdd_2, bdd_3, bdd_4, bdd_5)
  
  saveRDS(bdd, "A-donnees/donnees_propres.Rds")
  
}

rm(generer_variables_par_strate)

# summary(bdd)
# str(bdd)
# round(prop.table(table(bdd$strate_vec)) * 100, 1) 

# Description des donnees :
# - 5 variables auxiliaires x
# - strate_vec prend 2 modalites : "A" (Hors Idf) et "B" (Idf)
# - on prend les variables "_1", les autres cas c'est pour prendre en compte différents degrés d'homogénéité