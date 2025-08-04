# Y a-t-il un intérêt à combiner des échantillons issus d’une même enquête par sondage, mais tirés différemment ?

> **Stage réalisé par Thomas Guinhut, élève de deuxième année à l'Ensai**\
> 📅 *19 mai - 1er août 2025, division Sondages (DMCSI)*

------------------------------------------------------------------------

## 🎯 Objectifs du stage

Recherche, application et comparaison par simulations Monte-Carlo des méthodes de combinaison de plusieurs échantillons issus d'enquêtes par sondage, pour tenter d'améliorer la qualité des estimations nationales et régionales habituellement réalisées à partir d'un seul échantillon.

Travaux menés à la suite de ceux de Khaled Larbi pour l'**Enquête Nationale sur les Logements (ENL)**, grande enquête apériodique et historique de l'Insee dont l'édition 2023-2024 a la particularité d'avoir été multimode (entretiens par téléphone, internet ou face-à-face) et composée de plusieurs échantillons, dont l'un, d'une taille plus grande que les autres, surreprésente la région Île-de-France avec des réponses collectées par internet.

### 📈 Méthodologie

Au total, **10 estimateurs** applicables à l'ENL ont été identifiés. Ils sont comparés entre eux, ainsi qu'à des estimateurs classiques utilisés sur un seul échantillon pour voir si la combinaison est bénéfique.

Chaque estimateur est testé selon **3 méthodes de correction de la non-réponse** :
- Non-réponse corrigée avec les vraies probabilités de réponse
- Non-réponse corrigée avec une estimation des probabilités de réponse
- Non-réponse corrigée avec des groupes homogènes de réponse (GRH), construits à partir des estimations de probabilités de réponse
*Les estimateurs sont également analysés lorsqu'il n'y a pas de non-réponse.*

Différents scénarios de non-réponse peuvent être testés selon l'endogénéité de la réponse pour chaque mode, c'est-à-dire selon sa corrélation avec les variables d'intérêt :

| Scénario | Endogénéité totale | Endogénéité internet |
|:--------:|:------------------:|:--------------------:|
|    1     |         ❌         |          ❌          |
|    2     |         ❌         |        ✅ ✅         |
|    3     |         ✅         |        ✅ ✅         |
|    4     |       ✅ ✅        |        ✅ ✅         |

### 🔍 Contexte ENL

Dans le cas de l'ENL, il est possible de se ramener à **deux échantillons** :
- Un premier échantillon avec des réponses multimode
- Un deuxième avec des réponses collectées en monomode internet, avec surreprésentation des logements situés en région Île-de-France

Pour simplifier la modélisation, le multimode porte ici sur la collecte par internet et par téléphone, avec une préférence pour le téléphone.

------------------------------------------------------------------------

## 🚀 Utilisation du dépôt

> ⚠️ **Prérequis important**\
> Le projet est configuré de sorte qu'il soit utilisé sur une plateforme Onyxia (datalab ou LS3). En particulier, avant tout lancement de programme, il est nécessaire de disposer dans son bucket MinIO d'un dossier nommé `stage_2a_enl`, avec à l'intérieur la base de sondage simulée nommée `donnees_brut.parquet`.
>
> bucket/
> └── stage_2a_enl/
>     └── donnees_brut.parquet

### 📋 Installation, configuration et utilisation du projet

-   [ ] **Étape 1** : Cloner le dépôt Git, ou coller le lien dans les paramètres d'un nouveau service Onyxia
```bash
git clone https://github.com/thomasguinhut/stage_2a_enl
```
-   [ ] **Étape 2** : Ouvrir le projet R Studio `stage_2a_enl` dans le dossier du même nom qui a été importé
-   [ ] **Étape 3** : Ouvrir et lancer l'intégralité du script **`0-installation.R`** (**Ctrl+A**, puis **Ctrl+Entrée**) pour charger les packages, les données et les fonctions
-   [ ] **Étape 4** : Ouvrir et lancer l'intégralité du script **`1-parametrage.R`** (**Ctrl+A**, puis **Ctrl+Entrée**) pour charger les variables d'environnement nécessaires au lancement des simulations et à l'exportation des graphiques et tableaux
-   [ ] **Étape 5** : Ouvrir et lancer pas à pas les différentes lignes du script **`2-main.R`** pour, au choix :
    -   Faire un test des fonctions sur un seul échantillon
    -   Lancer un grand nombre de simulations et exporter les résultats graphiques dans MinIO

------------------------------------------------------------------------

## 📁 Organisation du dossier R

### 🗂️ Structure des dossiers

| Dossier | Description |
|------------------------------|------------------------------------------|
| **A** | 🔧 Fonctions pour importer configurer l'environnement de travail et charger les données simulées |
| **B** | 📊 Fonctions de tirage d'échantillons, de traitement de la non-réponse et d'estimations (classiques et par combinaison des deux échantillons) |
| **C** | 🧪 Application des fonctions importées en B pour un seul tirage. Ces scripts ont pour but de voir si tout marche bien, le code pour faire n simulations étant directement écrit dans main.R avec la fonction boucles_simulations |

### Indications

Le dossier R ne contient que des fonctions importées ensuite dans l'environnement R Studio en lancant le script **`0-installation.R`**. Après avoir ajouter les paramètres avec **`1-parametrage.R`**, toutes ces fonctions sont appelées dans **`2-main.R`** et il n'est pas nécessaire d'aller dans le dossier R, si ce n'est pour modifier une fonction.

En particulier, les fonctons du sous-dossier C sont appelées dans la rubrique "Test" de **`2-main.R`**. La rubrique suivante permet de généraliser à n simulations avec la fonction **`boucles_simulations`**.

------------------------------------------------------------------------

*Ce projet s'appuie en très grande partie sur les travaux antérieurs de Khaled Larbi, notamment pour la constitution de la base de sondage simulée et l'implémentation des fonctions de base de tirage d'échantillons et de correction de la non-réponse. Je le remercie pour la partage de ses travaux et son investissement durant ces trois mois, qui m'ont permis d'approfondir considérablement mon sujet de stage.*
