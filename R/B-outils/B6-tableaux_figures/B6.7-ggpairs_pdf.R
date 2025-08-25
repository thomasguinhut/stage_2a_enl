ggpairs_pdf <- function(strate = TRUE, 
                        var_names = NULL,
                        alpha_points = 0.3,
                        title_size = 14,
                        axis_text_size = 8,
                        colors_strate = c("#E74C3C", "#1E8449"),
                        text_size_strate = 2) {
  
  library(GGally)
  library(ggplot2)
  library(dplyr)
  
  bdd_test <- bdd %>%
    dplyr::select(x_1, x_2, x_3, x_4, x_5, z_1, z_2, z_3, y_1_total, y_2_total, y_3_total, strate_vec) %>%
    sample_n(10000) %>% 
    mutate(strate_vec = ifelse(strate_vec == "A", "IdF", "RdF"))
  
  nouveaux_noms <- c(
    "italic(X[1])","italic(X[2])","italic(X[3])","italic(X[4])","italic(X[5])",
    "italic(Z[1])","italic(Z[2])","italic(Z[3])","italic(Y[1])","italic(Y[2])","italic(Y[3])"
  )
  
  var_names <- nouveaux_noms
  
  if (!is.null(var_names)) {
    if (length(var_names) != 11) {
      stop("Le vecteur var_names doit contenir exactement 11 noms de variables")
    }
    names(bdd_test)[1:11] <- var_names
  }
  
  if (strate) {
    p_all <- ggpairs(
      data = bdd_test,
      columns = 1:11,
      mapping = aes(color = strate_vec, fill = strate_vec),
      upper = list(continuous = wrap("cor", size = text_size_strate)),  # taille contrôlée ici
      lower = list(continuous = wrap("points", alpha = alpha_points, size = 0.8)),
      diag = list(continuous = wrap("densityDiag", alpha = 0.7)),
      labeller = "label_parsed"
    )
  } else {
    p_all <- ggpairs(
      data = bdd_test,
      columns = 1:11,
      upper = list(continuous = wrap("cor", size = 4)),
      lower = list(continuous = wrap("points", alpha = alpha_points, size = 0.8)),
      diag = list(continuous = wrap("densityDiag", alpha = 0.7)),
      labeller = "label_parsed"
    )
  }
  
  # Pas de décimales
  p_all <- p_all + 
    scale_x_continuous(labels = function(x) sprintf("%.0f", x)) +
    scale_y_continuous(labels = function(x) sprintf("%.0f", x))
  
  # Appliquer les couleurs à la fois pour color et fill
  if (strate & !is.null(colors_strate)) {
    p_all <- p_all + 
      scale_color_manual(values = colors_strate) +
      scale_fill_manual(values = colors_strate)  # pour la diagonale
  }
  
  # Taille des titres/axes
  p_all <- p_all + theme(
    strip.text.x = element_text(size = title_size, face = "bold.italic"),
    strip.text.y = element_text(size = title_size, face = "bold.italic", angle = 0),
    axis.text = element_text(size = axis_text_size),
    axis.title = element_text(size = axis_text_size)
  )
  
  ggsave(
    filename = "test.png",
    plot = p_all,
    width = 210,
    height = 275,
    units = "mm",
    dpi = 300,
    device = "png"
  )
  
  message("La matrice de graphiques a été enregistrée sous 'test.png'.")
}