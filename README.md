
# Objectifs du stage

Recherche, application et comparaison par simulations Monte-Carlo des méthodes de combinaison de plusieurs échantillons issus d’enquêtes par sondage, pour tenter d’améliorer la qualité des estimations nationales et régionales habituellement réalisées à partir d’un seul échantillon.

Travaux menés à la suite de ceux de Khaled Larbi pour l’Enquête Nationale sur les Logements (ENL), grande enquête apériodique et historique de l’Insee dont l’édition 2023-2024 a la particularité d’avoir été multimode (entretiens par téléphone, internet ou face-à-face) et composée de plusieurs échantillons, dont l’un, d'une taille plus grande que les autres, surreprésente la région Île-de-France avec des réponses collectées par internet.

Au total, 10 estimateurs applicables à l'ENL ont été identifiés. Ils sont comparés entre eux, ainsi qu’à des estimateurs classiques utilisés sur un seul échantillon pour voir si la combinaison est bénéfique. Chaque estimateur est testé selon 3 méthodes de correction de la non-réponse : non-réponse corrigée avec les vraies probabilités de réponse, non-réponse corrigée avec une estimation des probabilités de réponse, et non-réponse corrigée avec des groupes homohènes de réponse (GRH), construits à partir des estimations de probabilités de réponse. Le estimateurs sont également analysés lorqu'il n'y a pas de non-réponse.

Différents scénarios de non-réponse peuvent être testés selon l'endogénéité de la réponse pour chaque mode, c'est-à-dire selon sa corrélation avec les variables d'intérêt :

| Scénario | Endogénéité totale | Endogénéité internet |
|----------|--------------------|----------------------|
| 1        | ❌                 | ❌                   |
| 2        | ❌                 | ✅ ✅                |
| 3        | ✅                 | ✅ ✅                |
| 4        | ✅ ✅              | ✅ ✅                |

Dans le cas de l'ENL, il est possible de se ramener à deux échantillons (un premier échantillon avec des réponses multimode et un deuxième avec des réponses collectées en monomode internet, avec surreprésentation des logements situés en région Île-de-France). Nous nous plaçons dans ce cas de figure pour réaliser ici les simulations de combinaison d'échantillons. Pour simplifier la modélisation, le multimode porte ici sur la collecte par internet et par téléphone, avec une préférence pour le téléphone.

*Stage réalisé par Thomas Guinhut, élève de deuxième année à l'Ensai, du 19 mai au 1er août 2025 à la division Sondages.*

# Utilisation du dépôt

> ⚠️ **Attention :** Le projet est configuré de sorte qu'il soit utilisé sur une plateforme Onyxia (datalab ou LS3). En particulier, il est nécéssaire de disposer dans son bucket MinIO d'un dossier nommé "stage_2a_enl", avec à l'intérieur la base de sondage simulée nommée "donnees_brut.parquet".

-   [ ] Cloner le dépôt Git, ou coller le lien dans les paramètres d'un nouveau service Onyxia.
-   [ ] Ouvrir le projet R Studio "stage_2a_enl" dans le dossier du même nom qui a été importé.
-   [ ] Ouvrir et lancer l'intégralité du script **0-installation.R** (**Ctrl+A**, puis **Ctrl+Entrée**) pour charger les packages, les données et les fonctions. Ce script charge également un nouveau thème pour R Studio ; pour désactiver cette option, mettez la deuxième ligne de ce scrit en commentaire.
-   [ ] Ouvrir et lancer l'intégralité du script **1-parametrage.R** (**Ctrl+A**, puis **Ctrl+Entrée**) pour charger les variables d'environnement nécessaires au lancement des simulations et à l'exportation des graphiques et tableaux.
-   [ ] Ouvrir et lancer pas à pas les différentes lignes du script **2-main.R** pour, au choix, faire un test des fonctions sur un seul échantillon, ou lancer un grand nombre de simulations et exporter les résultats dans MinIO.

# Organisation du dossier R

-   **A** - Fonctions pour importer configurer l'environnement de travail et charger les données simulées
-   **B** - Fonctions de tirage d'échantillons, de traitement de la non-réponse et d'estimations (classiques et par combinaison des deux échantillons)
-   **C** - Application des fonctions importées en B pour un seul tirage. Ces scripts ont pour but de voir si tout marche bien, le code pour faire n simulations étant directement écrit dans main.R avec la fonction boucles_simulations.

