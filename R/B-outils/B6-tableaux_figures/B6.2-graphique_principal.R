graphique_principal <- function(df, nb_sim, nom_methodes, nom_dossier, num_dossier, sc, chemin_sous_dossier, type_graphique, aws,
                                xmin_boxplots = NULL, xmax_boxplots = NULL, xmax_biais = NULL, xmax_eqm = NULL) {
  
  # Sélection des données selon le type de graphique
  if (type_graphique == "boxplots") {
    data_to_use <- df %>% 
      filter(is.na(scenario_nr) | scenario_nr == sc)
  } else if (type_graphique %in% c("biais", "eqm")) {
    data_to_use <- read.csv(paste0(chemin_sous_dossier, "/1-tableau_resultats_", nom_dossier, "_scenario_", sc, ".csv"))
  } else {
    stop("type_graphique doit être 'boxplots', 'biais' ou 'eqm'")
  }
  
  # Définitions des labels méthodes
  methode_labels <- c(
    "sans_nr"     = "SANS NR",
    "cnr_exacte"  = "CNR EXACTE",
    "sans_grh"    = "CNR P ESTIMÉS",
    "avec_grh"    = "GRH VINGTILES"
  )
  
  vertical_label <- function(x) {
    sapply(x, function(s) {
      mots <- strsplit(s, " ")[[1]]
      mots_verticaux <- sapply(mots, function(m) paste(strsplit(m, "")[[1]], collapse = "\n"))
      paste(mots_verticaux, collapse = "\n\n")
    })
  }
  
  # Nettoyage et transformation
  data_clean <- data_to_use %>%
    mutate(
      methode = factor(methode, levels = names(methode_labels)),
      methode_label = methode_labels[as.character(methode)],
      methode_vertical = sapply(methode_label, vertical_label),
      methode_vertical = factor(methode_vertical, levels = sapply(methode_labels, vertical_label)),
      
      y = recode_factor(
        y,
        "y_1_" = "Variable y_1<br><span style='font-size:10pt'>(distribution exponentielle)</span>",
        "y_2_" = "Variable y_2<br><span style='font-size:10pt'>(distribution parabolique de forme U)</span>",
        "y_3_" = "Variable y_3<br><span style='font-size:10pt'>(distribution gaussienne)</span>",
        "y_1" = "Variable y_1<br><span style='font-size:10pt'>(distribution exponentielle)</span>",
        "y_2" = "Variable y_2<br><span style='font-size:10pt'>(distribution parabolique de forme U)</span>",
        "y_3" = "Variable y_3<br><span style='font-size:10pt'>(distribution gaussienne)</span>",
        .ordered = TRUE
      ),
      
      estimateur = factor(estimateur),
      
      ensemble = case_when(
        ensemble == "total"    ~ "France ",
        ensemble == "strate_A" ~ "Hors Île-de-France ",
        ensemble == "strate_B" ~ "Île-de-France",
        TRUE                   ~ as.character(ensemble)
      ),
      
      ensemble = factor(ensemble, levels = c("France ", "Hors Île-de-France ", "Île-de-France"))
    )
  
  noms_estimateurs <- c(
    "Horvitz-Thompson sur multimode",
    "Horvitz-Thompson sur monomode",
    "Par expansion sur multimode",
    "Par expansion sur monomode",
    "Poids relatif échantillons (1a)",
    "Poids relatif répondants (1aprime)",
    "Partage de poids (1b)",
    "1a post-stratifié (2a)",
    "1aprime post-stratifié (2aprime)",
    "Bankier avec indépendance (3)",
    "Bankier sans indépendance (3prime)",
    "Bankier avec ind. CNR après (3a)",
    "Bankier sans ind. CNR ap. (3aprime)",
    "Bankier avec ind. CNR avant (3b)",
    "Bankier sans ind. CNR av. (3bprime)",
    "Elliott et Davis (4)"
  )
  
  # Recodage simple
  data_clean$estimateur <- recode(data_clean$estimateur,
                                  HT_multi            = noms_estimateurs[1],
                                  HT_mono             = noms_estimateurs[2],
                                  multimode_expansion = noms_estimateurs[3],
                                  monomode_expansion  = noms_estimateurs[4],
                                  `1a`                = noms_estimateurs[5],
                                  `1aprime`           = noms_estimateurs[6],
                                  `1b`                = noms_estimateurs[7],
                                  `2a`                = noms_estimateurs[8],
                                  `2aprime`           = noms_estimateurs[9],
                                  `3`                 = noms_estimateurs[10],
                                  `3prime`            = noms_estimateurs[11],
                                  `3a`                = noms_estimateurs[12],
                                  `3aprime`           = noms_estimateurs[13],
                                  `3b`                = noms_estimateurs[14],
                                  `3bprime`           = noms_estimateurs[15],
                                  `4`                 = noms_estimateurs[16]
  )
  
  # Filtrage spécifique pour le graphique EQM et ajustement des levels
  if (type_graphique == "eqm" && any(c("3a", "3b") %in% nom_methodes)) {
    estimateurs_a_exclure <- c(
      "Bankier avec indépendance des tirages (3)",
      "Bankier avec indépendance et CNR après (3a)",
      "Bankier avec indépendance et CNR avant (3b)"
    )
    data_clean <- data_clean %>%
      filter(!estimateur %in% estimateurs_a_exclure)
    
    # Ajuster les levels pour ne garder que ceux présents dans les données, dans le bon ordre
    noms_estimateurs_filtres <- noms_estimateurs[!noms_estimateurs %in% estimateurs_a_exclure]
    data_clean$estimateur <- factor(data_clean$estimateur, levels = noms_estimateurs_filtres)
  } else {
    # Facteur ordonné avec tous les estimateurs
    data_clean$estimateur <- factor(data_clean$estimateur, levels = noms_estimateurs)
  }
  
  # Choix variable et label selon type_graphique
  if (type_graphique == "boxplots") {
    var_to_plot <- "biais_relatif"
    y_label <- paste0(
      "\n\nIntervalles interquartiles et moyennes empiriques des\n",
      "biais relatifs exprimés en % sur les résultats de ", nb_sim, " simulations"
    )
    
    # Calcul des quantiles uniquement si nécessaire
    need_q25 <- is.null(xmin_boxplots)
    need_q75 <- is.null(xmax_boxplots)
    
    if (need_q25 || need_q75) {
      quantiles <- data_clean %>%
        group_by(methode, y, estimateur, ensemble) %>%
        summarise(
          q25 = quantile(biais_relatif, 0.25, na.rm = TRUE),
          q75 = quantile(biais_relatif, 0.75, na.rm = TRUE),
          .groups = "drop"
        )
    }
    
    xmin <- if (is.null(xmin_boxplots)) {
      floor(min(quantiles$q25, na.rm = TRUE) * 2) / 2
    } else {
      xmin_boxplots
    }
    
    xmax <- if (is.null(xmax_boxplots)) {
      ceiling(max(quantiles$q75, na.rm = TRUE) * 2) / 2
    } else {
      xmax_boxplots
    }
  } else if (type_graphique == "biais") {
    var_to_plot <- "biais_relatif_abs"
    y_label <- paste0("\n\nBiais relatifs absolus exprimés en % sur les résultats de ", nb_sim, " simulations")
    
    xmin <- 0
    
    if (is.null(xmax_biais)) {
      xmax <- max(data_clean[[var_to_plot]], na.rm = TRUE)
    } else {
      xmax <- xmax_biais
    }
    
  } else if (type_graphique == "eqm") {
    var_to_plot <- "cv_reqm"
    y_label <- paste0("\n\nCoefficients de variation de la REQM sur les résultats de ", nb_sim, " simulations")
    
    xmin <- 0
    
    if (is.null(xmax_eqm)) {
      xmax <- max(data_clean[[var_to_plot]], na.rm = TRUE)
    } else {
      xmax <- xmax_eqm
    }
    
  } else {
    stop("type_graphique doit être 'boxplots', 'biais' ou 'eqm'")
  }
  
  data_clean$ensemble <- factor(
    data_clean$ensemble,
    levels = rev(levels(data_clean$ensemble))
  )
  
  create_plot <- function(data) {
    data <- data %>%
      mutate(
        estimateur = fct_rev(droplevels(estimateur))
      )
    
    old_warn <- getOption("warn")
    options(warn = -1) 
    
    if (type_graphique == "boxplots") {
      p <- suppressWarnings({
        data %>%
          ggplot(aes(x = !!sym(var_to_plot), y = estimateur, fill = ensemble)) +
          geom_boxplot(
            width = 0.4,
            position = position_dodge(width = 0.65),
            outlier.shape = NA,
            coef = 0,
            color = "black",
            linewidth = 0.325,
            fatten = NULL
          ) +
          stat_summary(
            fun = mean,
            geom = "point",
            shape = 16,
            size = 1,
            color = "black",
            position = position_dodge(width = 0.65)
          ) +
          geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.45)
      })
    } else {
      p <- data %>%
        ggplot(aes(x = !!sym(var_to_plot), y = estimateur, fill = ensemble)) +
        geom_col(
          width = 0.4,
          position = position_dodge(width = 0.65),
          color = "black",
          linewidth = 0.325
        ) +
        geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.45)
    }
    
    p <- p +
      facet_grid(rows = vars(methode_vertical), cols = vars(y), scales = "free_y") +
      coord_cartesian(xlim = c(xmin, xmax)) +
      scale_fill_manual(
        values = c(
          "France " = "#5D6D7E",
          "Hors Île-de-France " = "#A9DFBF",
          "Île-de-France" = "#E74C3C"
        ),
        breaks = c("France ", "Hors Île-de-France ", "Île-de-France"),
        guide = guide_legend(reverse = FALSE)
      ) +
      theme(
        strip.text.y = element_text(lineheight = 0.9, size = 14, face = "bold", color = "black", hjust = 0.5, vjust = 0.5, angle = 0),
        strip.text.x = element_markdown(size = 14, face = "bold", color = "black", margin = margin(b = 2, t = 5)),
        strip.background = element_rect(fill = "#AFAFAF"),
        axis.text.y = element_text(size = 12),  # légèrement augmenté
        axis.text.x = element_text(size = 14),  # légèrement augmenté
        axis.title.y = element_blank(),        # supprime le titre de l'axe y
        axis.title.x = element_blank(),
        legend.text = element_text(size = 14),  # augmente taille des labels de légende
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.spacing.x = unit(0.7, "cm"),
        plot.margin = margin(0, 0, 0, 0)
      ) +
      labs(
        x = y_label
      )
    
    return(p)
  }
  
  # Début des graphiques
  pdf.options(encoding = "UTF-8")
  
  if (type_graphique == "boxplots") {
    
    # Complet
    p_all <- create_plot(data_clean)
    export_pdf(
      plot = p_all,
      filename = paste0("2-boxplots-complet_", nom_dossier, "_scenario_", sc, ".pdf"),
      width = 11,
      height = 15.5,
      aws = aws,
      chemin_sous_dossier = chemin_sous_dossier
    )
    
    # Page 1
    p_page1 <- create_plot(filter(data_clean, methode_label %in% methode_labels[c("sans_nr", "cnr_exacte")]))
    export_pdf(
      plot = p_page1,
      filename = paste0("2-boxplots-page1_", nom_dossier, "_scenario_", sc, ".pdf"),
      width = 11,
      height = 8,
      aws = aws,
      chemin_sous_dossier = chemin_sous_dossier
    )
    
    # Page 2
    p_page2 <- create_plot(filter(data_clean, methode_label %in% methode_labels[c("sans_grh", "avec_grh")]))
    export_pdf(
      plot = p_page2,
      filename = paste0("2-boxplots-page2_", nom_dossier, "_scenario_", sc, ".pdf"),
      width = 11,
      height = 8,
      aws = aws,
      chemin_sous_dossier = chemin_sous_dossier
    )
    
  } else if (type_graphique == "biais") {
    
    # Complet
    p_all <- create_plot(data_clean)
    export_pdf(
      plot = p_all,
      filename = paste0("3-biais-complet_", nom_dossier, "_scenario_", sc, ".pdf"),
      width = 11,
      height = 15.5,
      aws = aws,
      chemin_sous_dossier = chemin_sous_dossier
    )
    
    # Page 1
    p_page1 <- create_plot(filter(data_clean, methode_label %in% methode_labels[c("sans_nr", "cnr_exacte")]))
    export_pdf(
      plot = p_page1,
      filename = paste0("3-biais-page1_", nom_dossier, "_scenario_", sc, ".pdf"),
      width = 13,
      height = 8,
      aws = aws,
      chemin_sous_dossier = chemin_sous_dossier
    )
    
    # Page 2
    p_page2 <- create_plot(filter(data_clean, methode_label %in% methode_labels[c("sans_grh", "avec_grh")]))
    export_pdf(
      plot = p_page2,
      filename = paste0("3-biais-page2_", nom_dossier, "_scenario_", sc, ".pdf"),
      width = 13,
      height = 8,
      aws = aws,
      chemin_sous_dossier = chemin_sous_dossier
    )
    
  } else if (type_graphique == "eqm") {
    
    # Page 2 uniquement
    p_page2 <- create_plot(filter(data_clean, methode_label %in% methode_labels[c("sans_grh", "avec_grh")]))
    export_pdf(
      plot = p_page2,
      filename = paste0("4-eqm-page2_", nom_dossier, "_scenario_", sc, ".pdf"),
      width = 13,
      height = 8,
      aws = aws,
      chemin_sous_dossier = chemin_sous_dossier
    )
    
  } else {
    stop("type_graphique doit être 'boxplots', 'biais' ou 'eqm'")
  }
  
}