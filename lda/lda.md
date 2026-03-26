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

### Définition et utilité des paramètres

- **k (nombre de thèmes/topics)**
  Nombre de thèmes latents que le modèle va chercher dans le corpus.
  - `k` faible : thèmes larges/généraux.
  - `k` élevé : thèmes plus fins, mais parfois moins stables/interprétables.

- **n_terms (mots affichés par thème)**
  Nombre de mots les plus probables affichés pour décrire chaque thème.
  Ce paramètre n'affecte pas l'entraînement du modèle, uniquement l'affichage.

- **Retirer les stopwords**
  Supprime les mots-outils très fréquents (ex. « le », « de », « et ») pour améliorer la lisibilité des thèmes.

- **Filtrage morphosyntaxique**
  Conserve uniquement certaines catégories grammaticales (NOM, VER, ADJ, etc.) selon `lexique_fr.csv`.
  Utile pour concentrer l'analyse sur les termes porteurs de sens (souvent NOM/VER/ADJ).

## Paramètres LDA avancés (masqués dans l'UI simplifiée)

L'interface actuelle masque volontairement certains paramètres techniques pour rester simple.
Ils existent toutefois dans la fonction `lancer_test_lda()` :

- **iter (nombre d'itérations)**
  Nombre total d'itérations de l'échantillonnage Gibbs.
  Plus `iter` est élevé, plus l'estimation peut être stable (au prix d'un temps de calcul plus long).

- **burnin**
  Nombre d'itérations initiales ignorées.
  Sert à laisser la chaîne de Markov se stabiliser avant de collecter les échantillons utiles.

- **thin**
  Intervalle de sous-échantillonnage des itérations (ex. garder 1 état toutes les `thin` itérations).
  Sert à réduire l'autocorrélation entre échantillons successifs.

- **seed**
  Graine aléatoire pour la reproductibilité.
  Même corpus + mêmes paramètres + même seed => résultats plus reproductibles.

- **alpha**
  Hyperparamètre de Dirichlet sur la distribution topic/document.
  Plus faible = documents concentrés sur peu de topics ; plus élevé = documents plus mixtes.

- **eta (delta)**
  Hyperparamètre de Dirichlet sur la distribution mot/topic.
  Plus faible = topics plus spécialisés (quelques mots dominants) ; plus élevé = topics plus diffus.

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
