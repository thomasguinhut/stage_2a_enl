if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, data.table, survey, torch, pbivnorm, sampling, doParallel, foreach, base,
  parallel, stringr, forcats, ggplot2, ggthemes, tidyverse, ggh4x, ggtext, gridExtra, 
  tidyr, purrr, grid, patchwork, pbapply, future.apply, future, doFuture, progressr
)

exceptions <- c(
  "bdd", "sigma_1", "resultats",
  ls(pattern = "biais$"),
  ls(pattern = "brut$"),
  ls(pattern = "taux_rep_grh$"),
  ls(pattern = "poids_moyens$")
)

exceptions_df <- exceptions[sapply(exceptions, function(obj) {
  exists(obj, envir = .GlobalEnv) && 
    (
      is.data.frame(get(obj, envir = .GlobalEnv)) || 
        data.table::is.data.table(get(obj, envir = .GlobalEnv)) || 
        is.matrix(get(obj, envir = .GlobalEnv))
    )
})]

rm(list = setdiff(ls(envir = .GlobalEnv), exceptions_df), envir = .GlobalEnv)

