# Test LDA (Latent Dirichlet Allocation)

Un module de test LDA est disponible dans `iramuteqlite/lda.R`.
Un onglet **LDA** dans l'application permet aussi de lancer le test via une interface.

## Fonctions

- `preparer_dtm_lda(...)` : prépare une matrice terme-document à partir d'un vecteur de textes.
- `lancer_test_lda(...)` : entraîne un modèle LDA (Gibbs) et retourne :
  - `modele` : objet `topicmodels::LDA`
  - `top_terms` : top termes par topic
  - `doc_topics` : distribution des topics par document
  - `dtm` : matrice utilisée

## Paramètres utilisateur (onglet LDA)

Le bouton **Paramètres LDA** permet de modifier les paramètres principaux :

- Modèle : `k`, `n_terms`, `iter`, `burnin`, `thin`, `seed`, `alpha`, `eta`
- Pré-traitement DTM : `langue`, `min_termfreq`, `remove_numbers`, `remove_punct`, `remove_symbols`, `stopwords_sup`

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
