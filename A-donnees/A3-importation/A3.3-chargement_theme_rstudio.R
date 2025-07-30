library(jsonlite)

auto_load_rstudio_theme <- function(theme_file = "mytheme.rstheme", theme_name = "Pebble-dark") {
  # 1. Détecter le chemin du dossier des thèmes selon OS
  sysname <- Sys.info()[["sysname"]]
  if (sysname == "Windows") {
    theme_dir <- file.path(Sys.getenv("LOCALAPPDATA"), "RStudio", "themes")
    config_dir <- file.path(Sys.getenv("LOCALAPPDATA"), "RStudio-Desktop", "monitored", "user-settings")
  } else {
    theme_dir <- file.path(Sys.getenv("HOME"), ".config", "rstudio", "themes")
    if (!dir.exists(theme_dir)) {
      theme_dir <- file.path(Sys.getenv("HOME"), ".local", "share", "rstudio", "themes")
    }
    config_dir <- file.path(Sys.getenv("HOME"), ".config", "rstudio", "monitored", "user-settings")
  }
  
  # 2. Créer les dossiers s'ils n'existent pas
  if (!dir.exists(theme_dir)) dir.create(theme_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(config_dir)) dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 3. Copier le fichier de thème dans le dossier themes
  dest_theme_file <- file.path(theme_dir, paste0(theme_name, ".rstheme"))
  if (!file.exists(dest_theme_file) || 
      tools::md5sum(theme_file) != tools::md5sum(dest_theme_file)) {
    file.copy(theme_file, dest_theme_file, overwrite = TRUE)
  }
  
  # 4. Modifier le fichier user-settings.json pour appliquer le thème
  user_settings_file <- file.path(config_dir, "user-settings.json")
  settings <- list()
  if (file.exists(user_settings_file)) {
    settings <- fromJSON(user_settings_file)
  }
  
  if (is.null(settings$editor_theme) || settings$editor_theme != theme_name) {
    settings$editor_theme <- theme_name
    write_json(settings, user_settings_file, pretty = TRUE, auto_unbox = TRUE)
  }
}

# Appelle la fonction automatiquement au lancement
auto_load_rstudio_theme(theme_file = "mytheme.rstheme", theme_name = "Pogomba")
