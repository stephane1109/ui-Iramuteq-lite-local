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

## Paramètres utilisateur (onglet LDA, simplifiés)

Le bouton **Paramètres LDA** permet de modifier simplement :

- Modèle : `k`, `n_terms`
- Pré-traitement DTM : `langue`, `min_termfreq`, `remove_numbers`, `remove_punct`, `stopwords_sup`

L'onglet affiche aussi un **graphique** des mots les plus probables par thème (barres par topic), en plus des tableaux.

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
