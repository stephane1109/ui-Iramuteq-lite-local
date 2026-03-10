# Rapport de faisabilité — Refonte UI IRaMuTeQ-lite (Shiny + bslib)

## Objectif
Proposer une interface plus moderne et proche des usages d'IRaMuTeQ :

1. **Menu principal horizontal** (navigation claire par grandes étapes).
2. **Boîtes de dialogue (modales)** pour les paramétrages au lieu d'un long panneau latéral.
3. **Explorateur à gauche** avec l'historique des corpus importés et les analyses produites.
4. **Socle technique explicite :** `library(shiny)` + `library(bslib)`.

---

## Faisabilité globale

✅ **Faisabilité : élevée** (refonte UX/UI progressive, sans réécrire le moteur d'analyse).

L'application actuelle est déjà structurée autour de Shiny et de modules/fichiers métiers (`iramuteqlite/*.R`), ce qui permet une migration UI incrémentale.

### Pourquoi c'est faisable
- Le moteur de calcul (CHD/AFC/nettoyage) semble séparé de l'affichage, donc on peut **changer le contenant UI** sans casser le fond.
- Shiny gère nativement les modales (`showModal`, `modalDialog`) et les layouts dynamiques.
- `bslib` permet un thème moderne, composants Bootstrap 5 et une meilleure cohérence visuelle.

### Risques principaux
- Couplage possible entre certains contrôles UI et logique serveur (ids d'inputs historiques).
- Gestion d'état : passage d'inputs visibles en permanence à des formulaires en modale.
- Complexité de l'explorateur (arborescence corpus → analyses → artefacts) si l'historique n'est pas encore persisté.

---

## Proposition d'architecture UI

## 1) Structure d'écran

- **Header / Navbar horizontale**
  - Corpus
  - Prétraitement
  - CHD
  - AFC
  - Résultats
  - Aide

- **Colonne gauche fixe (explorateur)**
  - Corpus importés (date, nom fichier, statut)
  - Analyses associées (CHD, AFC, statistiques)
  - Actions rapides (ouvrir, renommer, supprimer, exporter)

- **Zone centrale**
  - Vue contextuelle selon l'onglet actif (tableaux, graphes, sortie texte, etc.)

- **Panneau droit optionnel**
  - Résumé de session (paramètres actifs, logs, progression)

## 2) Paramétrage via modales

Au lieu de 50+ champs en sidebar, créer des boutons :
- « Paramètres CHD »
- « Nettoyage »
- « Dictionnaire »
- « Paramètres AFC »

Chaque bouton ouvre une modale avec :
- formulaire organisé en sections,
- bouton **Appliquer**,
- bouton **Réinitialiser défauts**,
- validation des entrées.

## 3) Système de thème avec bslib

- Base : `bs_theme(version = 5, bootswatch = "flatly"|"minty"|"lux")`
- Palette proche d'un outil analytique pro (bleu/gris, accent secondaire).
- Uniformiser : boutons, cartes, tableaux, alertes, badges de statut.

---

## Plan de bataille (implémentation)

## Phase 0 — Cadrage (0,5 jour)
- Cartographier les `inputId` actuels et leurs dépendances serveur.
- Lister ce qui doit rester stable pour éviter de casser le backend.

**Livrable :** matrice `inputId` actuel → nouvelle localisation UI.

## Phase 1 — Fondation visuelle (1 jour)
- Introduire `bslib` dans `ui.R`.
- Poser un `page_navbar()` (ou `navset_*`) horizontal.
- Définir thème global, typographie, spacing, composants de base.

**Livrable :** shell applicatif moderne sans régression fonctionnelle.

## Phase 2 — Explorateur gauche (1 à 2 jours)
- Créer un composant « Explorateur » (module recommandé).
- Afficher : corpus + analyses dérivées.
- Ajouter interactions minimales (sélection / focus résultat).

**Livrable :** navigation projet-like par corpus et analyses.

## Phase 3 — Migration des paramètres en modales (2 jours)
- Remplacer les gros blocs de sidebar par boutons d'ouverture de modales.
- Extraire formulaires en fonctions UI dédiées par domaine (CHD, nettoyage, AFC, dictionnaire).
- Sauvegarder/apply des options via `reactiveValues`.

**Livrable :** UX plus légère, proche du comportement IRaMuTeQ visé.

## Phase 4 — Qualité et robustesse (1 jour)
- Vérifier non-régression sur flux complet : import → CHD → AFC → affichage.
- Ajouter indicateurs visuels : progression, succès/erreur, logs utiles.
- Ajustements responsive (écran portable).

**Livrable :** version stable prête à test utilisateur.

## Phase 5 — Finitions (0,5 à 1 jour)
- Polissage visuel (icônes, contrastes, densité informationnelle).
- Harmoniser wording FR et microcopies.

**Livrable :** UI « sexy » et cohérente.

---

## Estimation effort

- **Refonte MVP (sans persistance avancée)** : **5 à 7 jours**.
- **Version plus poussée** (historique persistant, gestion multi-projets, exports enrichis) : **8 à 12 jours**.

---

## Détails techniques recommandés

## Dépendances minimales
```r
library(shiny)
library(bslib)
```

## Squelette UI cible (exemple)
```r
ui <- page_navbar(
  title = "IRaMuTeQ-lite",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  nav_panel("Corpus", ...),
  nav_panel("Prétraitement", ...),
  nav_panel("CHD", ...),
  nav_panel("AFC", ...),
  nav_panel("Résultats", ...),
  nav_panel("Aide", ...)
)
```

## Exemple modal paramètres
```r
observeEvent(input$open_chd_settings, {
  showModal(modalDialog(
    title = "Paramètres CHD",
    numericInput("segment_size", "Taille segment", value = 40, min = 5),
    numericInput("min_docfreq", "Fréquence minimale", value = 3, min = 1),
    footer = tagList(
      modalButton("Annuler"),
      actionButton("apply_chd_settings", "Appliquer", class = "btn-primary")
    )
  ))
})
```

---

## Recommandations UX concrètes

- Prioriser un **workflow en étapes** plutôt qu'une liste de paramètres brute.
- Conserver des **préréglages** (ex. « IRaMuTeQ compatible », « exploratoire », « rapide »).
- Afficher un **résumé compact des paramètres actifs** près du bouton « Lancer analyse ».
- Pour l'explorateur, utiliser une nomenclature stable :
  - `Corpus_YYYYMMDD_HHMM`
  - `CHD_01`, `AFC_01`, `NUAGES_01`, etc.

---

## Décision go/no-go

✅ **Go recommandé.**

La demande est techniquement réaliste, améliore fortement l'ergonomie, et peut être livrée de façon progressive sans refonte totale du moteur analytique.
