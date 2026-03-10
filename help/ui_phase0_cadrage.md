# Phase 0 — Cadrage UI v2 (menus horizontaux + modales + explorateur)

## Objectif de la phase 0
Mettre à plat les dépendances UI/serveur avant refonte, pour garantir une migration sans régression.

## Livrables produits
- `help/ui_phase0_inventory.csv` : inventaire des `inputId` déclarés dans l'UI + nombre de références serveur.
- `help/ui_phase0_matrice_input_ids.csv` : matrice de migration `inputId actuel -> zone cible UI v2` + criticité.

## Résultat du cadrage
- **34 `inputId`** recensés côté UI.
- Les paramètres les plus couplés au serveur (à verrouiller en priorité) :
  - `max_p`, `supprimer_chiffres`, `supprimer_ponctuation`,
  - `fichier_corpus`, `lancer`,
  - bloc CHD avancé (`k_iramuteq`, `iramuteq_mincl_mode`, `iramuteq_classif_mode`).
- Une matrice de relocalisation est prête pour la future UI v2 (navbar horizontale + modales thématiques).

## Écarts détectés à traiter en phase 1
- `afc_top_termes` et `afc_top_modalites` sont utilisés côté serveur mais non visibles dans l'UI actuelle (utilisation défensive via tests `is.null`).
- `afc_brush` est un `brushOpts(id=...)` (normalement hors inventaire des contrôles classiques).
- `dl_zip` est déclaré UI, mais hors dépendance `input$...` (flux download standard Shiny).

## Règles de migration fixées
1. **Conserver les `inputId` existants** pour éviter de casser `server_events_lancer_iramuteq.R`.
2. **Déplacer sans renommer** les contrôles vers modales par domaine (CHD / Nettoyage / Dictionnaire / AFC).
3. **Valider les dépendances conditionnelles** avant bascule (ex. `filtrage_morpho -> pos_lexique_a_conserver`).
4. **Geler les IDs critiques** jusqu'à la fin des tests de non-régression.

## Découpage de migration validé
- Navbar horizontale : `Corpus`, `Prétraitement`, `CHD`, `AFC`, `Résultats`, `Aide`.
- Explorateur gauche : corpus importés + analyses produites.
- Modales de paramétrage :
  - Paramètres CHD (général + avancé IRaMuTeQ)
  - Nettoyage
  - Dictionnaire
  - Paramètres AFC

## Commandes utilisées pour établir le cadrage
```bash
rg 'input\$[A-Za-z0-9_]+' -n
python - <<'PY'
# Extraction des inputId déclarés dans ui.R + ui_options_iramuteq.R
# puis comptage de leurs références dans app.R et iramuteqlite/*.R
PY
```
