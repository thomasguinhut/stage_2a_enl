# Nettoyage de l'environnement
rm(list = ls())

# Fermeture de toutes les fenêtres graphiques ouvertes
while (dev.cur() > 1) dev.off()