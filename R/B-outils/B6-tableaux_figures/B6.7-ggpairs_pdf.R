ggpairs_pdf <- function() {
  
  # Sélection des variables
  bdd_test <- bdd %>%
    dplyr::select(x_1, x_2, x_3, x_4, x_5, z_1, z_2, z_3, y_1_total, y_2_total, y_3_total)
  
  # Génération de la matrice de graphiques
  p_all <- ggpairs(bdd_test)
  
  # Sauvegarde en PDF
  ggsave(
    filename = "test.pdf",
    plot = p_all,
    width = 11,
    height = 15.5,
    device = cairo_pdf
  )
  
  message("La matrice de graphiques a été enregistrée sous 'test.pdf'.")
}