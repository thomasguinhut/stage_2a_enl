export_resultats <- function(resultats, chiffre, chemin_dossier, nom_dossier) {
  
  # Envoie vers S3 (brut)
  aws.s3::s3write_using(
    resultats$brut,
    FUN = readr::write_csv,
    object = paste0(chemin_dossier, "/", chiffre, ".1-brut_", nom_dossier, ".csv"),
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )
  
  # Envoie vers S3 (biais)
  aws.s3::s3write_using(
    resultats$biais,
    FUN = readr::write_csv,
    object = paste0(chemin_dossier, "/", chiffre, ".2-biais_", nom_dossier, ".csv"),
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )
  
  # Envoie vers S3 (taux de réponse GRH)
  aws.s3::s3write_using(
    resultats$taux_rep_grh,
    FUN = readr::write_csv,
    object = paste0(chemin_dossier, "/", chiffre, ".3-taux_rep_grh_", nom_dossier, ".csv"),
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )
  
}
