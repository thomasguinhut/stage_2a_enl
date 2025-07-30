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
  
  if (parallel) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      stop("Le package 'parallel' est requis pour l'exécution en parallèle.")
    }
    
    if (is.null(n_cores)) n_cores <- 8
    n_cores <- max(1, min(n_cores, nb_sim, 20)) 
    
    if (is.null(batch_size)) {
      batch_size <- max(1, ceiling(nb_sim / (n_cores * 5)))
    }
    
    sim_indices <- seq_len(nb_sim)
    batches <- split(sim_indices, ceiling(seq_along(sim_indices) / batch_size))
    
    cat("=== CONFIGURATION PARALLÈLE ===\n")
    cat(sprintf("Cœurs utilisés : %d\n", n_cores))
    cat(sprintf("Taille base : %.2f MB\n", as.numeric(object.size(bdd)) / 1024^2))
    cat(sprintf("Nombre de simulations : %d en %d batches (taille batch ~%d)\n", 
                nb_sim, length(batches), batch_size))
    
    temp_files <- list()
    
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
    
    cl <- parallel::makeCluster(n_cores)
    on.exit({
      parallel::stopCluster(cl)
      for (f in temp_files) if (file.exists(f)) unlink(f)
    }, add = TRUE)
    
    light_vars <- c("nom_methodes", "n_multi", "n_mono",
                    "part_strate_A_dans_mode_1", "part_strate_A_dans_mode_2",
                    "scenarios", "prefix_var_interet", "strat_var_name",
                    "modele_latent", "formule_cnr", "grh", "taux_min_grh", 
                    "source_files", "temp_files")
    
    parallel::clusterExport(cl, light_vars, envir = environment())
    
    parallel::clusterEvalQ(cl, {
      gc()
      pacman::p_load(dplyr, data.table, survey, sampling, stringr)
      lapply(source_files, source)
      if (!is.null(temp_files$bdd)) bdd <- readRDS(temp_files$bdd)
      if (!is.null(temp_files$sigma)) sigma <- readRDS(temp_files$sigma)
      gc()
    })
    
    process_batch <- function(batch_indices) {
      batch_results <- list()
      batch_taux_rep <- list()
      batch_poids <- list()
      batch_errors <- character(0)
      
      cat(sprintf("[Worker] Début batch avec simulations %d à %d\n", min(batch_indices), max(batch_indices)))
      batch_start <- Sys.time()
      
      for (sim_idx in batch_indices) {
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
          
          batch_results[[as.character(sim_idx)]] <- result$brut
          batch_taux_rep[[as.character(sim_idx)]] <- result$taux_rep_grh_mode
          batch_poids[[as.character(sim_idx)]] <- result$poids_cnr
          rm(result)
          gc()
        }, error = function(e) {
          batch_errors <<- c(batch_errors, sprintf("Erreur simulation %d: %s", sim_idx, e$message))
        })
      }
      
      batch_time <- as.numeric(difftime(Sys.time(), batch_start, units = "secs"))
      cat(sprintf("[Worker] Fin batch %d-%d en %s\n", min(batch_indices), max(batch_indices), format_temps(batch_time)))
      
      # Sauvegarde batch sur disque pour éviter surcharge RAM
      res_file <- tempfile(pattern = paste0("sim_batch_", min(batch_indices), "_", max(batch_indices), "_"), fileext = ".rds")
      saveRDS(list(brut = batch_results, taux_rep = batch_taux_rep, poids = batch_poids, errors = batch_errors), file = res_file)
      gc()
      
      return(res_file)
    }
    
    parallel::clusterExport(cl, "process_batch", envir = environment())
    
    cat("→ Lancement des simulations parallèles...\n")
    batch_result_files <- parallel::parLapplyLB(cl, batches, process_batch)
    
    cat("\n→ Reconstruction des résultats finaux...\n")
    
    res <- vector("list", nb_sim)
    taux_reps_grh_mode <- vector("list", nb_sim)
    poids_moyens_cnr <- vector("list", nb_sim)
    all_errors <- character(0)
    successful_sims <- 0
    
    for (res_file in batch_result_files) {
      batch_res <- readRDS(res_file)
      if (length(batch_res$errors) > 0) all_errors <- c(all_errors, batch_res$errors)
      
      for (sim_idx_str in names(batch_res$brut)) {
        sim_idx <- as.numeric(sim_idx_str)
        res[[sim_idx]] <- batch_res$brut[[sim_idx_str]]
        taux_reps_grh_mode[[sim_idx]] <- batch_res$taux_rep[[sim_idx_str]]
        poids_moyens_cnr[[sim_idx]] <- batch_res$poids[[sim_idx_str]]
        successful_sims <- successful_sims + 1
      }
      
      unlink(res_file)
      gc()
    }
    
    cat(sprintf("\n✓ Total : %d/%d simulations réussies\n", successful_sims, nb_sim))
    if (length(all_errors) > 0) {
      cat(sprintf("\n⚠️ %d erreurs détectées :\n", length(all_errors)))
      for (err in all_errors) cat(sprintf("  • %s\n", err))
    }
    
  } else {
    # mode séquentiel (non parallèle)
    res <- vector("list", nb_sim)
    taux_reps_grh_mode <- vector("list", nb_sim)
    poids_moyens_cnr <- vector("list", nb_sim)
    
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
      gc()
    }
  }
  
  t_total <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("\nTemps total écoulé : %s\n", format_temps(t_total)))
  
  return(list(brut = res,
              taux_rep_grh_mode = taux_reps_grh_mode,
              poids_moyens_cnr = poids_moyens_cnr))
}
