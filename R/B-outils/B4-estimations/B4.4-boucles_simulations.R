source("R/B-outils/B4-estimations/B4.3-lancer_une_simulation.R")

boucles_simulations <- function(nb_sim,
                                bdd,
                                nom_methodes,
                                n_multi,
                                n_mono,
                                part_strate_A_dans_mode_1,
                                part_strate_A_dans_mode_2,
                                scenarios,
                                prefix_var_interet,
                                strat_var_name,
                                sigma,
                                modele_latent = TRUE,
                                formule_cnr,
                                grh = 20,
                                taux_min_grh,
                                parallel = FALSE,
                                n_cores = NULL,
                                batch_size = NULL,
                                use_disk_cache = TRUE,
                                sauvegarde_param = FALSE,
                                source_files = c(
                                  "R/B-outils/B4-estimations/B4.3-lancer_une_simulation.R",
                                  "R/B-outils/B2-correction_non_reponse/B2.3-cnr_finale.R",
                                  "R/B-outils/B1-tirage/B1.5-tirage.R"
                                )) {
  format_temps <- function(secs) {
    mins <- floor(secs / 60)
    sec <- round(secs %% 60)
    sprintf("%d min %d s", mins, sec)
  }
  
  t0 <- Sys.time()
  res <- vector("list", nb_sim)
  taux_reps_grh_mode <- vector("list", nb_sim)
  poids_moyens_cnr <- vector("list", nb_sim)
  
  original_threads <- data.table::getDTthreads()
  
  if(sauvegarde_param){
    params <- as.list(match.call())
  } else {
    params <- list()
  }
  
  start_time <- Sys.time()
  if (parallel) {
    
    if (is.null(n_cores)) {
      n_cores <- max(1, parallel::detectCores() - 2)
    }
    
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    doParallel::registerDoParallel(cl)
    
    # Export des objets et fichiers nécessaires
    parallel::clusterExport(cl, varlist = c("source_files", "bdd", "nom_methodes", "n_multi", "n_mono",
                                            "part_strate_A_dans_mode_1", "part_strate_A_dans_mode_2",
                                            "scenarios", "prefix_var_interet", "strat_var_name", "sigma",
                                            "modele_latent", "formule_cnr", "grh", "taux_min_grh"), envir = environment())
    
    parallel::clusterEvalQ(cl, {
      library(dplyr)
      library(data.table)
      data.table::setDTthreads(1)
      for (file in source_files) {
        source(file)
      }
    })
    
    # Exécution parallèle
    res_sim <- foreach(i = 1:nb_sim,
                       .packages = c("dplyr", "data.table", "survey", "sampling", "stringr"),
                       .errorhandling = "remove") %dopar% {
                         cat(sprintf("Simulation %d sur %d\n", i, nb_sim))
                         
                         result <- lancer_une_simulation(
                           i = i,
                           bdd = bdd,
                           nom_methodes = nom_methodes,
                           n_multi = n_multi,
                           n_mono = n_mono,
                           part_strate_A_dans_mode_1 = part_strate_A_dans_mode_1,
                           part_strate_A_dans_mode_2 = part_strate_A_dans_mode_2,
                           scenarios = scenarios,
                           prefix_var_interet = prefix_var_interet,
                           strat_var_name = strat_var_name,
                           sigma = sigma,
                           modele_latent = modele_latent,
                           formule_cnr = formule_cnr,
                           grh = grh,
                           taux_min_grh = taux_min_grh
                         )
                         
                         list(
                           brut = result$brut,
                           taux_rep_grh_mode = result$taux_rep_grh_mode,
                           poids_cnr = result$poids_cnr
                         )
                       }
    
    # Récupération des résultats
    for (i in seq_along(res_sim)) {
      res[[i]] <- res_sim[[i]]$brut
      taux_reps_grh_mode[[i]] <- res_sim[[i]]$taux_rep_grh_mode
      poids_moyens_cnr[[i]] <- res_sim[[i]]$poids_cnr
    }
  } else {
    for (i in seq_len(nb_sim)) {
      cat(sprintf("Simulation %d/%d\n", i, nb_sim))
      result <- lancer_une_simulation(
        i = i,
        bdd = bdd,
        nom_methodes = nom_methodes,
        n_multi = n_multi,
        n_mono = n_mono,
        part_strate_A_dans_mode_1 = part_strate_A_dans_mode_1,
        part_strate_A_dans_mode_2 = part_strate_A_dans_mode_2,
        scenarios = scenarios,
        prefix_var_interet = prefix_var_interet,
        strat_var_name = strat_var_name,
        sigma = sigma,
        modele_latent = modele_latent,
        formule_cnr = formule_cnr,
        grh = grh,
        taux_min_grh = taux_min_grh
      )
      res[[i]] <- result$brut
      taux_reps_grh_mode[[i]] <- result$taux_rep_grh_mode
      poids_moyens_cnr[[i]] <- result$poids_cnr
    }
  }
  
  t_total <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  temps_moyen <- t_total / nb_sim
  
  cat("\n=== RESULTATS ===\n")
  cat(sprintf("Temps total: %s\n", format_temps(t_total)))
  cat(sprintf("Temps moyen par simulation: %s\n", format_temps(temps_moyen)))
  
  res_final_brut <- dplyr::bind_rows(res) %>%
    dplyr::arrange(
      desc(is.na(scenario_nr)),
      scenario_nr,
      y,
      ensemble,
      desc(is.na(methode)),
      factor(methode, levels = c("sans_nr", "cnr_exacte", "sans_grh", "avec_grh")),
      estimateur,
      simulation
    ) %>%
    filter(
      !(estimateur %in% c("4", "4A", "4B")) |
        (estimateur == "4" & ensemble == "total") |
        (estimateur == "4A" & ensemble == "strate_A") |
        (estimateur == "4B" & ensemble == "strate_B")
    ) %>%
    mutate(
      estimateur = ifelse(estimateur %in% c("4", "4A", "4B"), "4", estimateur)
    ) %>%
    dplyr::group_by(y, ensemble, estimateur) %>%
    dplyr::filter(!(estimateur == "recensement" & dplyr::row_number() > 1)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      estimateur = dplyr::case_when(
        methode %in% c("cnr_exacte", "sans_grh", "avec_grh") & estimateur == "HT_multi" ~ "multimode_expansion",
        methode %in% c("cnr_exacte", "sans_grh", "avec_grh") & estimateur == "HT_mono" ~ "monomode_expansion",
        TRUE ~ as.character(estimateur)
      ),
      estimateur = factor(estimateur, levels = c(
        "HT_multi", "HT_mono", "multimode_expansion",
        "monomode_expansion", nom_methodes, "3", "3prime", "recensement"
      )),
      methode = factor(methode, levels = c("sans_nr", "cnr_exacte", "sans_grh", "avec_grh"))
    )
  
  res_final_biais <- res_final_brut %>%
    dplyr::group_by(y, ensemble) %>%
    dplyr::mutate(biais_relatif = ifelse(
      !grepl("recensement", nom),
      round(((somme_ponderee - somme_ponderee[grepl("recensement", nom)]) /
               somme_ponderee[grepl("recensement", nom)]) * 100, 2),
      somme_ponderee
    )) %>%
    dplyr::ungroup() %>%
    dplyr::filter(estimateur != "recensement")
  
  taux_rep_grh <- dplyr::bind_rows(taux_reps_grh_mode) %>%
    group_by(scenario, grh, mode) %>%
    summarise(taux_rep = mean(taux_rep), .groups = "drop")
  
  return(list(
    brut = res_final_brut,
    biais = res_final_biais,
    taux_rep_grh = taux_rep_grh
  ))
}
