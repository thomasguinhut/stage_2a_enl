options(renv.config.sandbox.enabled = FALSE,
        encoding = "UTF-8")

.libPaths(c("/home/onyxia/work/userLibrary", .libPaths()))

invisible(Sys.setlocale("LC_CTYPE", "C.utf8"))

if (requireNamespace("rstudioapi", quietly = TRUE)) {
  if (rstudioapi::isAvailable()) {
    try({
      rstudioapi::applyTheme("Tomorrow Night 80s")  # Change ici par ton thème préféré
    }, silent = TRUE)
  }
}

