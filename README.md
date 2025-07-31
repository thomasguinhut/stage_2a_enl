# Objectif du stage

Recherche, application et comparaison des méthodes de combinaison d’échantillons issus d’enquêtes par sondage, pour tenter d’améliorer la qualité des estimations nationales et régionales habituellement réalisées à partir d’un seul échantillon.

Travaux menés à la suite de ceux de Khaled Larbi pour l’Enquête Nationale sur les Logements (ENL), grande enquête apériodique et historique de l’Insee dont l’édition 2023-2024 a la particularité d’avoir été multimode (entretiens par téléphone, internet ou face-à-face) et composée de plusieurs échantillons, dont l’un surreprésente la région Île-de-France avec des réponses collectées par internet.

Au total, 9 estimateurs applicables à l'ENL ont été identifiés. Ils sont comparés entre eux, ainsi qu’à des estimateurs classiques utilisés sur un seul échantillon pour voir si la combinaison est bénéfique.

Dans le cas de l'ENL, il est possible de se ramener à deux échantillons (un premier échantillon avec des réponses multimode et un deuxième avec des réponses collectées en monomode internet, avec surreprésentation des logements situés en région Île-de-France). Nous nous plaçons dans ce cas de figure pour réaliser ici les simulations de combinaison d'échantillons.

*Stage réalisé par Thomas Guinhut, élève de deuxième année à l'Ensai, du 19 mai au 1er août 2025 à la division Sondages.*

# Utilisation du dépôt

> ⚠️ **Attention :** Le projet est configuré de sorte qu'il soit utilisé sur une plateforme Onyxia (datalab ou LS3)

-   [ ] Ouvrir le fichier **main.R**
-   [ ] Lancer l'intégralité du script (**Ctrl+A**, puis **Ctrl+Entrée**), **OU** lancer pas à pas les différentes lignes du script pour gérer volontairement les différentes étapes du projet, notamment si vous utilisez la partie **C** pour faire un test des fonctions sur un seul échantillon

# Organisation du dépôt

-   **A** - Fonctions pour importer configurer l'environnement de travail et chager les données simulées
-   **B** - Fonctions de tirage d'échantillons, de traitement de la non-réponse et d'estimations (classiques et par combinaison des deux échantillons)
-   **C** - Application des fonctions importées en B (les scripts d'application stockés dans le dossier **C** ont pour but de voir si tout marche bien avec une seule simulation, le code pour faire n simulations est directement écrit dans la partie **C** du script main.R)
-   **D** (dossier stocké uniquement en local) - Exports des fichiers de simulations et des graphiques en PDF

Le script main.R permet d'importer les fonctions de **A** et **B**. Elles sont ensuites appliquées dans la partie **C**, pour une simulation sur un seul ou n échantillons.
