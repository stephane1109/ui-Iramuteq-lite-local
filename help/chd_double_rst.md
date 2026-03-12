# CHD double avec RST1 et RST2 — explication

Ce document explique **le mode de classification `double`** et le rôle des paramètres **`rst1`** / **`rst2`** dans IRaMuTeQ-lite.

## 1) À quoi servent RST1 et RST2 ?

- **RST** = taille de segment (en nombre de tokens) utilisée pour découper les textes en UCE/segments.
- En mode **simple**, on applique une seule segmentation.
- En mode **double**, l'application crée **deux segmentations indépendantes** du corpus source :
  1. une segmentation avec `rst1`,
  2. une segmentation avec `rst2`.

Ces deux jeux de segments sont ensuite **concaténés** pour alimenter l'analyse CHD.

---

## 2) Pipeline exact du mode double

Dans le code, la fonction `split_segments_double_rst(...)` suit cette logique :

1. Validation de `rst1` et `rst2` (valeurs entières positives, défauts 12 et 14 si invalide).
2. `split_segments(corpus, segment_size = rst1, ...)` sur le corpus d'origine.
3. `split_segments(corpus, segment_size = rst2, ...)` **aussi sur le corpus d'origine**.
4. Concaténation des textes segmentés `rst1` + `rst2` en un corpus unique.
5. Ajout d'une variable `docvar` `rst_source` (`"rst1"` ou `"rst2"`) pour tracer la provenance des segments.

⚠️ Point important : on **ne** fait **pas** une re-segmentation `rst1 -> rst2`. Si on le faisait, `rst2` perdrait souvent son effet (notamment quand `rst2 > rst1`).

---

## 3) Lien avec la CHD (classification)

Une fois les segments produits :

- Le moteur CHD (`CHD(...)`) travaille sur la matrice segments × formes.
- En mode `double`, le paramètre de post-traitement `mincl` automatique utilise une convention plus stricte :
  - `ind = nbcl * 2` (au lieu de `nbcl` en simple),
  - `mincl = round(n_uce / ind)`.

Cela reflète le fait qu'on combine deux découpes du corpus dans la logique de classification.

---

## 4) Comment lire RST1 vs RST2 en pratique

- **`rst1` plus petit** (ex. 10–12) : segments plus courts, plus locaux, plus sensibles aux micro-thèmes.
- **`rst2` plus grand** (ex. 14–20) : segments plus longs, plus stables, plus contextuels.
- Le mode double mélange ces deux granularités pour renforcer la robustesse des classes.

Bon réglage de départ (souvent pertinent) :
- `rst1 = 12`
- `rst2 = 14`

Puis ajuster selon :
- longueur moyenne des textes,
- homogénéité du corpus,
- stabilité/interpretabilité des classes obtenues.

---

## 5) Résumé en une phrase

En **CHD double**, IRaMuTeQ-lite exécute **deux segmentations parallèles du corpus** (RST1 et RST2), les fusionne en un seul ensemble de segments, puis applique la CHD sur cet ensemble enrichi pour capturer à la fois des structures lexicales fines et plus globales.
