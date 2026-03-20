# Analyse de similitude de Vergès

## 1) Ce que c’est

L’analyse de similitude de Vergès est une méthode d’analyse lexicale qui construit un **réseau de mots** à partir de leurs cooccurrences dans le corpus.

- Chaque mot devient un **sommet**.
- Chaque lien entre deux mots devient une **arête**.
- La force du lien dépend d’un **indice** (selon la méthode choisie).

Le but est d’identifier la structure du discours : mots centraux, sous-ensembles thématiques, et liens entre thèmes.

---

## 2) Définitions essentielles

### Effectif
Nombre total d’occurrences d’un mot dans le corpus (ou dans le sous-corpus analysé).

### Indice
Valeur numérique utilisée pour mesurer la force d’association entre deux mots.

### Arête
Lien entre deux sommets lorsque leur association est conservée après calcul et filtrage.

### Sommet
Nœud du graphe représentant un mot (ou une unité lexicale).

---

## 3) Méthode de calcul : cooccurrence, Jaccard, binomiale

Dans l’interface, le champ **« Méthode de calcul »** propose trois choix.

### Cooccurrence
- Mesure directe : on compte dans combien d’unités textuelles deux mots apparaissent ensemble.
- Plus la cooccurrence est élevée, plus le lien est fort.
- Avantage : lecture simple et intuitive.
- Limite : favorise mécaniquement les mots très fréquents.

### Jaccard
- Mesure normalisée : compare l’intersection des contextes des deux mots à l’union de ces contextes.
- Donne un ratio d’association relatif, moins dépendant des seuls volumes bruts.
- Avantage : meilleure comparabilité entre paires de mots de fréquences différentes.
- Limite : peut sous-valoriser certains liens rares mais informatifs.

### Binomiale
- Mesure probabiliste : évalue si la cooccurrence observée est plus forte que ce qu’on attendrait par hasard compte tenu des fréquences.
- Avantage : met l’accent sur les associations statistiquement saillantes.
- Limite : plus technique à interpréter qu’un simple comptage.

---

## 4) Toutes les options de l’onglet « Analyse de similitude »

## 4.1 Paramètres de construction

### Méthode de calcul
Choix de l’indice utilisé pour pondérer les arêtes : **cooccurrence**, **Jaccard** ou **binomiale**.

### Seuil minimal des arêtes
- Supprime les liens trop faibles.
- Plus le seuil est élevé, plus le graphe est lisible mais plus on perd de relations secondaires.

### Nombre de termes à conserver
- Limite l’analyse aux mots les plus fréquents.
- Réduit le bruit et améliore la lisibilité du graphe.

### Termes à analyser
- Sélection manuelle d’un sous-ensemble de mots.
- Permet une analyse ciblée sur un champ lexical précis.

### Limiter au graphe couvrant maximal
- Conserve l’ossature des liens les plus forts pour garder un réseau connecté et lisible.
- Utile quand le graphe complet est trop dense.

---

## 4.2 Paramètres de visualisation

### Type de layout
Définit la géométrie d’affichage du graphe.

- **Fruchterman-Reingold** : placement par forces, souvent lisible pour des graphes généraux.
- **Kamada-Kawai** : autre algorithme par forces, parfois plus stable sur certaines structures.
- **Circulaire** : tous les sommets sur un cercle, utile pour inspection globale.
- **Aléatoire** : placement aléatoire, surtout utile pour comparaison ou tests rapides.
- **Spirale** : organisation en spirale, parfois utile pour grands graphes.

### Type d’affichage du graphe
- **Interactif (visNetwork)** : zoom, déplacement, exploration manuelle des nœuds et liens.
- **Statique (igraph avec halo)** : rendu fixe orienté impression/rapport.

### Afficher les labels des arêtes
Affiche la valeur du lien sur chaque arête (utile pour lecture précise, mais peut charger visuellement).

### Largeur des arêtes proportionnelle à l’indice
Épaisseur des liens proportionnelle à leur force d’association.

### Taille du texte des sommets proportionnelle aux fréquences
Agrandit les labels des mots les plus fréquents.

---

## 4.3 Paramètres de communautés

### Communautés
Active la détection de groupes de sommets fortement connectés.

### Méthode de communautés
Choix de l’algorithme de partition :
- edge.betweenness.community
- fastgreedy.community
- label.propagation.community
- leading.eigenvector.community
- multilevel.community
- walktrap.community

### Halo
Ajoute un effet visuel autour des communautés pour faciliter leur repérage.

---

## 5) Comment lire le résultat

- Un mot très connecté peut structurer un thème.
- Un groupe dense de mots peut indiquer un sous-thème cohérent.
- Un mot qui relie deux groupes peut jouer un rôle de pont thématique.
- Une arête très forte indique une association robuste dans le corpus.

L’interprétation doit toujours être vérifiée en revenant aux segments textuels concrets.
