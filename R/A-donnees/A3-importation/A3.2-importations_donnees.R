source("R/A-donnees/A2-preparation_donnees/A2.1-generer_variables_par_strate.R")

if (!(exists("sigma_1"))) {
  sigma_1 <- matrix(0.5, 2, 2)
  diag(sigma_1) <- 1
}

if (!(exists("bdd"))) {
  bdd_1 <- aws.s3::s3read_using(
    FUN = arrow::read_parquet,
    object = "stage_2a_enl/donnees_brut.parquet",
    bucket = "thomasguinhut",
    envir = .GlobalEnv,
    opts = list("region" = "")
  )

  bdd_2 <- bdd_1 %>%
    select(!starts_with("z"))

  bdd_3 <- generer_variables_par_strate(bdd_2, c("y_", "x_"))

  bdd_4 <- bdd_3 %>%
    mutate(recensement = 1)

  bdd_5 <- bdd_4 %>%
    rename(
      y_1_total = y_1,
      y_2_total = y_2,
      y_3_total = y_3,
      y_4_total = y_4,
      y_5_total = y_5,
      y_6_total = y_6,
      y_7_total = y_7,
      y_8_total = y_8,
      y_9_total = y_9,
      y_10_total = y_10
    )

  bdd <- bdd_5

  rm(bdd_1, bdd_2, bdd_3, bdd_4, bdd_5)
}

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
        (requireNamespace("data.table", quietly = TRUE) && data.table::is.data.table(get(obj, envir = .GlobalEnv))) ||
        is.matrix(get(obj, envir = .GlobalEnv))
    )
})]

rm(list = setdiff(ls(envir = .GlobalEnv), exceptions_df), envir = .GlobalEnv)
gc()
