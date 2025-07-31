export_pdf <- function(plot, filename, width, height, aws, chemin_sous_dossier) {
  if (!aws) {
    ggsave(
      filename = file.path(chemin_sous_dossier, filename),
      plot = plot,
      device = cairo_pdf,
      width = width,
      height = height,
      units = "in"
    )
  } else {
    # Crée un fichier temporaire avec l'extension .pdf
    temp_file <- tempfile(fileext = ".pdf")
    
    ggsave(
      filename = temp_file,
      plot = plot,
      device = cairo_pdf,
      width = width,
      height = height,
      units = "in"
    )
    
    # Construit le chemin complet pour le fichier sur S3
    full_path <- file.path(chemin_sous_dossier, filename)
    
    # Télécharge le fichier vers S3
    aws.s3::put_object(
      file = temp_file,
      object = full_path,
      bucket = "thomasguinhut",
      region = "us-east-1" # Remplacez par votre région si nécessaire
    )
    
    # Supprime le fichier temporaire après le téléchargement
    unlink(temp_file)
  }
}