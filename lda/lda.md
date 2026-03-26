# Test LDA (Latent Dirichlet Allocation)

Un module de test LDA est disponible dans `lda/lda.R`.
Un onglet **LDA** dans l'application permet aussi de lancer le test via une interface.
Les options UI sont définies dans `lda/ui_lda_iramuteq.R`.

## Fonctions

- `preparer_dtm_lda(...)` : prépare une matrice terme-document à partir d'un vecteur de textes.
- `lancer_test_lda(...)` : entraîne un modèle LDA (Gibbs) et retourne :
  - `modele` : objet `topicmodels::LDA`
  - `top_terms` : top termes par topic
  - `doc_topics` : distribution des topics par document
  - `dtm` : matrice utilisée

## Paramètres utilisateur (onglet LDA, très simplifiés)

Le bouton **Paramètres LDA** permet de modifier :

- `k` : nombre de thèmes
- `n_terms` : nombre de mots affichés par thème
- option de retrait des stopwords
- option de filtrage morphosyntaxique (`NOM`, `VER`, `ADJ`, etc.)

L'onglet affiche aussi :
- un **graphique en bulles** qui projette les topics (position issue d'une projection PCA, taille selon la prévalence moyenne),
- un **graphique barres** des mots les plus probables par topic,
- des tables document/topic et un tableau des segments de texte avec topic dominant.

Le LDA peut être lancé directement après **import du corpus**, sans lancer CHD/AFC.

## Exemple rapide

```r
resultat_lda <- lancer_test_lda(
  textes = c(
    "Le climat change et les émissions augmentent",
    "Les marchés financiers réagissent aux taux",
    "L'écologie urbaine devient centrale"
  ),
  k = 2,
  n_terms = 8,
  min_termfreq = 1
)

head(resultat_lda$top_terms)
head(resultat_lda$doc_topics)
```
