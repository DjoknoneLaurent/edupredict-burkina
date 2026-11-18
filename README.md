# EduPredict : cartographie et anticipation de l'abandon scolaire au Burkina Faso

Tableau de bord d'aide à la décision réalisé pour le **Défi 1 du Data Challenge Burkina 2026 (Édition Éducation)** :
« Où le risque de décrochage est-il le plus fort, par région, et comment l'anticiper ? »

## Tester l'application en ligne

**[Ouvrir le tableau de bord](https://djoknonelaurent.shinyapps.io/edupredict-burkina/)** : https://djoknonelaurent.shinyapps.io/edupredict-burkina/

Si l'application était en veille, elle redémarre en moins d'une minute.

## Ce que l'on y trouve

| Onglet | Question à laquelle il répond |
|---|---|
| L'essentiel | La situation en quatre chiffres et cinq messages |
| Carte du risque | Où le risque d'abandon est-il le plus fort, par région et par province, et pourquoi ? Carte d'alerte à quatre niveaux |
| Probabilités | Quelle est la probabilité qu'un élève abandonne, et qu'est-ce qui la fait varier (régression logistique, rapports de cotes, effets marginaux) ? |
| Fiches | Fiche d'une page en PDF pour chaque province et pour chaque directeur régional |
| Anticiper | Abandon attendu de 2024-2025 à 2029-2030 (curseur par année, trois scénarios), enfants privés d'école en 2030, provinces à surveiller avant qu'elles ne basculent |
| Plan d'action | Que faire, où, quand, avec qui et à quel coût |
| Méthode | Sources, méthodes, limites |

Au survol d'une région ou d'une province : constat, facteurs de risque et facteurs protecteurs chiffrés, recommandations.
Un clic agrandit la carte et épingle l'explication.

## Lancer l'application sur son ordinateur

1. Installer R (version 4.2 ou plus récente) : https://cran.r-project.org
2. Dans R, installer les paquets une seule fois :
   ```r
   install.packages(c("shiny", "bslib", "plotly", "DT", "dplyr", "tidyr", "readr", "jsonlite", "htmltools"))
   ```
3. Lancer directement depuis GitHub, sans rien télécharger :
   ```r
   shiny::runGitHub("edupredict-burkina", "DjoknoneLaurent")
   ```
   ou, après avoir téléchargé ce dépôt (bouton « Code » > « Download ZIP ») et ouvert R dans le dossier :
   ```r
   shiny::runApp()
   ```

Le dépôt est autonome : l'application (`app.R`, `global.R`, `fiche_pdf.R`, `www/`) et toutes les données qu'elle lit (`donnees/`) y sont.

## Données et méthodes, en bref

- **Sources** : annuaires statistiques de l'éducation 2016-2025 (DGESS, ministère de l'Éducation nationale), INSD, CONASUR,
  OCHA, limites administratives COD-AB. Données publiques, utilisées uniquement pour le concours.
- **Risque et facteurs** : associations robustes retenues par consensus de plusieurs méthodes ; poids des facteurs issus d'une régression Ridge.
- **Probabilités** : régression logistique fractionnaire, écarts-types regroupés par province, jugée sur dix critères fixés à l'avance
  (fiable pour décrire et comparer, pas pour prévoir).
- **Prévision** : niveau habituel de chaque province (médiane de ses années observées), méthode qui a fait mieux que tous les
  modèles testés ; fourchette tirée des erreurs passées ; scénarios conditionnels jusqu'en 2030.
- **Bascule** : analyse de survie sur l'histoire des 16 provinces où moins de 70 % des écoles fonctionnent.

Les résultats décrivent des liens observés, pas des causes prouvées.

## Contenu du dépôt

```
app.R          interface et logique du tableau de bord
global.R       lecture des données et textes d'interprétation
fiche_pdf.R    fiches d'une page (PDF)
www/carte.js   interactivité des cartes (survol, zoom, épinglage)
donnees/       les 37 fichiers lus par l'application (sorties du pipeline d'analyse)
```
