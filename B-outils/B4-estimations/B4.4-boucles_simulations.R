source("B-outils/B4-estimations/B4.3-lancer_une_simulation.R")

boucles_simulations <- function(nb_sim,
                                bdd = get0("bdd", envir = .GlobalEnv, ifnotfound = NULL),
                                nom_methodes,
                                n_multi,
                                n_mono,
                                part_strate_A_dans_mode_1,
                                part_strate_A_dans_mode_2,
                                scenarios = c("1", "2"),
                                prefix_var_interet = "y_1_",
                                strat_var_name = "strate_vec",
                                sigma = get0("sigma_1", envir = .GlobalEnv, ifnotfound = NULL),
                                modele_latent = TRUE,
                                formule_cnr = as.formula("rep_1 ~ x_1 + x_2 + x_3 + x_4 + x_5"),
                                grh = 20,
                                taux_min_grh,
                                parallel = FALSE,
                                n_cores = NULL,
                                batch_size = NULL,
                                use_disk_cache = TRUE,
                                source_files = c(
                                  "B-outils/B4-estimations/B4.3-lancer_une_simulation.R",
                                  "B-outils/B2-correction_non_reponse/B2.3-cnr_finale.R",
                                  "B-outils/B1-tirage/B1.5-tirage.R")) {
  
  if ("4" %in% nom_methodes) {
    pos <- which(nom_methodes == "4")
    additions <- c()
    if (!"4A" %in% nom_methodes) additions <- c(additions, "4A")
    if (!"4B" %in% nom_methodes) additions <- c(additions, "4B")
    if (length(additions) > 0) {
      nom_methodes <- append(nom_methodes, additions, after = pos)
    }
  }
  
  format_temps <- function(secs) {
    mins <- floor(secs / 60)
    sec <- round(secs %% 60)
    sprintf("%d min %d s", mins, sec)
  }
  
  t0 <- Sys.time()
  
  res <- vector("list", nb_sim)
  taux_reps_grh_mode <- vector("list", nb_sim)
  poids_moyens_cnr <- vector("list", nb_sim)
  
  if (parallel) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      stop("Le package 'parallel' est requis pour l'exécution en parallèle.")
    }
    
    # 🔒 Limiter manuellement le nombre de cœurs
    if (is.null(n_cores)) n_cores <- 8
    n_cores <- max(1, min(n_cores, nb_sim, 20))  # on limite à 20 max par défaut
    
    # 🧮 Taille des batches : beaucoup de petits batches pour lisser la charge
    if (is.null(batch_size)) {
      batch_size <- max(1, ceiling(nb_sim / (n_cores * 5)))
    }
    
    sim_indices <- seq_len(nb_sim)
    batches <- split(sim_indices, ceiling(sim_indices / batch_size))
    
    cat("=== CONFIGURATION PARALLÈLE ===\n")
    cat(sprintf("Cœurs utilisés : %d\n", n_cores))
    cat(sprintf("Taille bdd : %.2f MB\n", as.numeric(object.size(bdd)) / 1024^2))
    cat(sprintf("Nombre de simulations : %d en %d batches (taille moyenne ~%d)\n", 
                nb_sim, length(batches), batch_size))
    
    temp_files <- list()
    
    # 💾 Cache disque : bdd
    if (use_disk_cache && !is.null(bdd)) {
      cat("→ Cache disque pour bdd\n")
      temp_files$bdd <- tempfile(fileext = ".rds")
      saveRDS(bdd, temp_files$bdd)
      rm(bdd)
      gc()
    }
    
    if (use_disk_cache && !is.null(sigma)) {
      cat("→ Cache disque pour sigma\n")
      temp_files$sigma <- tempfile(fileext = ".rds")
      saveRDS(sigma, temp_files$sigma)
      rm(sigma)
      gc()
    }
    
    # 🛠️ Initialisation cluster
    cl <- parallel::makeCluster(n_cores)
    on.exit({
      parallel::stopCluster(cl)
      for (f in temp_files) if (file.exists(f)) unlink(f)
    }, add = TRUE)
    
    # 🔃 Variables légères transmises aux workers
    light_vars <- c("nom_methodes", "n_multi", "n_mono",
                    "part_strate_A_dans_mode_1", "part_strate_A_dans_mode_2",
                    "scenarios", "prefix_var_interet", "strat_var_name",
                    "modele_latent", "formule_cnr", "grh", "taux_min_grh", 
                    "source_files", "temp_files")
    
    parallel::clusterExport(cl, light_vars, envir = environment())
    
    # 🧠 Setup des workers
    parallel::clusterEvalQ(cl, {
      gc()
      pacman::p_load(dplyr, data.table, survey, sampling, stringr)
      lapply(source_files, source)
      if (!is.null(temp_files$bdd)) bdd <- readRDS(temp_files$bdd)
      if (!is.null(temp_files$sigma)) sigma <- readRDS(temp_files$sigma)
      gc()
    })
    
    # 🔄 Fonction batch avec nettoyage mémoire
    process_batch <- function(batch_indices) {
      batch_results <- list()
      batch_taux_rep <- list()
      batch_poids <- list()
      batch_errors <- character(0)
      
      for (sim_idx in batch_indices) {
        cat(sprintf("[Worker] Simulation %d - mémoire avant :\n", sim_idx))
        print(gc())
        
        tryCatch({
          result <- lancer_une_simulation(
            i = sim_idx,
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
          
          # ⛏️ Option : réduire taille du résultat
          batch_results[[as.character(sim_idx)]] <- result$brut
          batch_taux_rep[[as.character(sim_idx)]] <- result$taux_rep_grh_mode
          batch_poids[[as.character(sim_idx)]] <- result$poids_cnr
          rm(result)
          gc()
          
        }, error = function(e) {
          batch_errors <<- c(batch_errors, sprintf("Erreur simulation %d: %s", sim_idx, e$message))
        })
      }
      
      gc()
      
      list(
        indices = batch_indices,
        brut = batch_results,
        taux_rep = batch_taux_rep,
        poids = batch_poids,
        errors = batch_errors
      )
    }
    
    parallel::clusterExport(cl, "process_batch", envir = environment())
    
    process_batch_with_progress <- function(batch_indices) {
      batch_num <- which(sapply(batches, function(b) identical(b, batch_indices)))
      cat(sprintf("[Worker] Début batch %d (%d simulations)...\n", batch_num, length(batch_indices)))
      batch_start <- Sys.time()
      result <- process_batch(batch_indices)
      batch_time <- as.numeric(difftime(Sys.time(), batch_start, units = "secs"))
      cat(sprintf("[Worker] ✓ Batch %d terminé en %.1f sec\n", batch_num, batch_time))
      result
    }
    
    parallel::clusterExport(cl, "process_batch_with_progress", envir = environment())
    
    cat("→ Lancement des simulations...\n")
    results_batched <- parallel::parLapplyLB(cl, batches, process_batch_with_progress)
    
    cat("\n→ Reconstruction des résultats\n")
    all_errors <- character(0)
    successful_sims <- 0
    
    for (i in seq_along(results_batched)) {
      batch_result <- results_batched[[i]]
      if (length(batch_result$errors) > 0) {
        all_errors <- c(all_errors, batch_result$errors)
      }
      batch_success_count <- length(batch_result$brut)
      successful_sims <- successful_sims + batch_success_count
      cat(sprintf("Batch %d: %d/%d simulations réussies\n", i, batch_success_count, length(batch_result$indices)))
      
      for (sim_idx_str in names(batch_result$brut)) {
        sim_idx <- as.numeric(sim_idx_str)
        res[[sim_idx]] <- batch_result$brut[[sim_idx_str]]
        taux_reps_grh_mode[[sim_idx]] <- batch_result$taux_rep[[sim_idx_str]]
        poids_moyens_cnr[[sim_idx]] <- batch_result$poids[[sim_idx_str]]
      }
      gc()
    }
    
    cat(sprintf("\n✓ Total : %d/%d simulations réussies\n", successful_sims, nb_sim))
    if (length(all_errors) > 0) {
      cat(sprintf("\n⚠️ %d erreurs détectées :\n", length(all_errors)))
      for (err in all_errors) cat(sprintf("  • %s\n", err))
    }
    
    cat("→ Taille des objets principaux :\n")
    cat("  res : "); print(object.size(res), units = "MB")
    cat("  taux_reps_grh_mode : "); print(object.size(taux_reps_grh_mode), units = "MB")
    cat("  poids_moyens_cnr : "); print(object.size(poids_moyens_cnr), units = "MB")
    gc()
  } else {
    # Pas de parallélisme (optionnel, tu peux garder ton ancienne logique)
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
      estimateur = factor(estimateur, levels = c("HT_multi", "HT_mono", "multimode_expansion",
                                                 "monomode_expansion", nom_methodes, "3", "3prime", "recensement")),
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
