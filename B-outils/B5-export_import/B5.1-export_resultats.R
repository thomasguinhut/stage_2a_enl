export_pdf <- function(plot, filename, width, height, aws, chemin_sous_dossier) {
  if (aws == FALSE) {
    ggsave(
      filename = filename,
      plot = plot,
      device = cairo_pdf,
      width = width,
      height = height,
      units = "in"
    )
  } else {
    # Construire le chemin complet dans le bucket, ex: "sous_dossier/monfichier.pdf"
    chemin_s3 <- file.path(chemin_sous_dossier, basename(filename))
    
    s3write_using(
      FUN = function(tempfile) {
        ggsave(
          filename = tempfile,
          plot = plot,
          device = cairo_pdf,
          width = width,
          height = height,
          units = "in"
        )
      },
      object = chemin_s3,
      bucket = "thomasguinhut"
    )
  }
}
