#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Script principal d'analyse LDA.
Ce script lit un fichier JSON d'entrée, prépare les unités d'analyse,
calcule un modèle LDA et écrit un JSON de sortie exploitable par Shiny.
"""

import argparse
import json
import os
import re
import sys
import traceback
import unicodedata
from dataclasses import dataclass
from typing import Dict, List

from sklearn.decomposition import LatentDirichletAllocation
from sklearn.feature_extraction.text import CountVectorizer

# Liste minimale de mots vides en français (complémentaire aux stop words anglais sklearn).
MOTS_VIDES_FR = {
    "a", "afin", "ai", "ainsi", "apres", "au", "aucun", "aussi", "autre", "aux", "avec",
    "avoir", "bon", "car", "ce", "cela", "ces", "ceux", "chaque", "ci", "comme", "comment",
    "dans", "de", "des", "du", "dedans", "dehors", "depuis", "devrait", "doit", "donc", "dos",
    "droite", "debut", "elle", "elles", "en", "encore", "essai", "est", "et", "eu", "fait",
    "faites", "fois", "font", "force", "haut", "hors", "ici", "il", "ils", "je", "juste",
    "la", "le", "les", "leur", "là", "ma", "maintenant", "mais", "mes", "mine", "moins",
    "mon", "mot", "meme", "ni", "nommes", "notre", "nous", "nouveaux", "ou", "où", "par",
    "parce", "parole", "pas", "personnes", "peut", "peu", "piece", "plupart", "pour", "pourquoi",
    "quand", "que", "quel", "quelle", "quelles", "quels", "qui", "sa", "sans", "ses", "seulement",
    "si", "sien", "son", "sont", "sous", "soyez", "sujet", "sur", "ta", "tandis", "tellement",
    "tels", "tes", "ton", "tous", "tout", "trop", "tres", "tu", "valeur", "voie", "voient",
    "vont", "votre", "vous", "vu", "ca", "etaient", "etat", "etions", "ete", "etre"
}


@dataclass
class UniteTexte:
    """Représente une unité d'analyse (document entier ou segment)."""

    identifiant: str
    texte: str
    type_unite: str


def charger_json(chemin_json: str) -> Dict:
    """Charge et valide un fichier JSON d'entrée."""
    with open(chemin_json, "r", encoding="utf-8") as flux:
        donnees = json.load(flux)

    if "corpus_texte" not in donnees:
        raise ValueError("Le champ 'corpus_texte' est obligatoire dans le JSON d'entrée.")

    return donnees


def decouper_documents(corpus_texte: str, marqueur: str = "****") -> List[str]:
    """Découpe un corpus en documents à partir d'un marqueur de début de document."""
    lignes = corpus_texte.splitlines()
    documents = []
    courant = []

    for ligne in lignes:
        if ligne.strip().startswith(marqueur):
            if courant:
                documents.append("\n".join(courant).strip())
                courant = []
            propre = ligne.strip()[len(marqueur):].strip()
            if propre:
                courant.append(propre)
        else:
            courant.append(ligne)

    if courant:
        documents.append("\n".join(courant).strip())

    documents = [d for d in documents if d and d.strip()]

    if not documents and corpus_texte.strip():
        # Fallback : si aucun marqueur n'est détecté, on considère tout le texte comme un document.
        documents = [corpus_texte.strip()]

    return documents


def segmenter_document(texte: str, longueur_min_segment: int) -> List[str]:
    """Segmente un document sur une ponctuation simple et filtre selon une longueur minimale."""
    fragments = re.split(r"[\.!\?;:\n]+", texte)
    segments_valides = []

    for fragment in fragments:
        segment = fragment.strip()
        if not segment:
            continue
        # Longueur minimale en nombre de caractères non blancs.
        if len(segment) >= longueur_min_segment:
            segments_valides.append(segment)

    return segments_valides


def construire_unites_analyse(
    documents: List[str],
    mode_unite: str,
    longueur_min_segment: int,
) -> List[UniteTexte]:
    """Construit les unités d'analyse selon le mode choisi."""
    unites = []

    if mode_unite == "document":
        for i, doc in enumerate(documents, start=1):
            unites.append(UniteTexte(identifiant=f"DOC_{i}", texte=doc, type_unite="document"))
    elif mode_unite == "segment":
        for i, doc in enumerate(documents, start=1):
            segments = segmenter_document(doc, longueur_min_segment)
            for j, segment in enumerate(segments, start=1):
                unites.append(
                    UniteTexte(
                        identifiant=f"DOC_{i}_SEG_{j}",
                        texte=segment,
                        type_unite="segment",
                    )
                )
    else:
        raise ValueError("Le mode d'unité doit être 'document' ou 'segment'.")

    return unites


def supprimer_accents(texte: str) -> str:
    """Supprime les accents d'une chaîne Unicode."""
    normalise = unicodedata.normalize("NFKD", texte)
    return "".join(c for c in normalise if not unicodedata.combining(c))


def tokeniser_francais(texte: str) -> List[str]:
    """Tokenise un texte avec des règles simples et filtre les mots vides."""
    texte = texte.lower()
    texte = supprimer_accents(texte)
    texte = re.sub(r"[^a-z\s']", " ", texte)
    candidats = re.findall(r"[a-z']{2,}", texte)

    tokens = []
    for mot in candidats:
        mot_propre = mot.strip("'")
        if not mot_propre:
            continue
        if mot_propre in MOTS_VIDES_FR:
            continue
        tokens.append(mot_propre)

    return tokens


def extraire_topics(
    modele_lda: LatentDirichletAllocation,
    noms_termes: List[str],
    nb_mots_par_topic: int,
) -> List[Dict]:
    """Transforme les composantes LDA en liste de topics lisible."""
    topics = []

    for indice_topic, poids_termes in enumerate(modele_lda.components_, start=1):
        indices_tries = poids_termes.argsort()[::-1][:nb_mots_par_topic]
        mots = []
        for indice in indices_tries:
            mots.append(
                {
                    "mot": noms_termes[indice],
                    "poids": float(poids_termes[indice]),
                }
            )

        topics.append(
            {
                "topic": indice_topic,
                "mots": mots,
            }
        )

    return topics


def analyser_lda(donnees: Dict) -> Dict:
    """Exécute toute la chaîne d'analyse LDA et retourne un dictionnaire résultat."""
    mode_unite = donnees.get("mode_unite", "document")
    longueur_min_segment = int(donnees.get("longueur_min_segment", 50))
    nb_topics = int(donnees.get("nb_topics", 5))
    nb_mots_par_topic = int(donnees.get("nb_mots_par_topic", 10))
    random_state = int(donnees.get("random_state", 42))
    corpus_texte = donnees["corpus_texte"]

    documents = decouper_documents(corpus_texte=corpus_texte, marqueur="****")
    if not documents:
        raise ValueError("Aucun document exploitable n'a été trouvé dans le corpus.")

    unites = construire_unites_analyse(
        documents=documents,
        mode_unite=mode_unite,
        longueur_min_segment=longueur_min_segment,
    )

    if not unites:
        raise ValueError(
            "Aucune unité d'analyse disponible après segmentation. "
            "Réduisez le seuil minimal de longueur des segments."
        )

    textes_unites = [u.texte for u in unites]

    vectoriseur = CountVectorizer(
        tokenizer=tokeniser_francais,
        preprocessor=None,
        lowercase=False,
        token_pattern=None,
        min_df=1,
        max_df=0.95,
        stop_words="english",
    )

    matrice_doc_termes = vectoriseur.fit_transform(textes_unites)
    if matrice_doc_termes.shape[1] == 0:
        raise ValueError("Aucun terme valide après prétraitement. Vérifiez le corpus.")

    if nb_topics > matrice_doc_termes.shape[0]:
        nb_topics = max(1, matrice_doc_termes.shape[0])

    modele_lda = LatentDirichletAllocation(
        n_components=nb_topics,
        random_state=random_state,
        learning_method="batch",
        max_iter=30,
    )

    distribution_topics_unites = modele_lda.fit_transform(matrice_doc_termes)

    noms_termes = vectoriseur.get_feature_names_out().tolist()
    topics = extraire_topics(modele_lda, noms_termes, nb_mots_par_topic)

    detail_unites = []
    for idx, unite in enumerate(unites):
        scores = distribution_topics_unites[idx]
        topic_dominant = int(scores.argmax() + 1)
        detail_unites.append(
            {
                "identifiant": unite.identifiant,
                "type_unite": unite.type_unite,
                "texte": unite.texte,
                "topic_dominant": topic_dominant,
                "distribution_topics": [float(x) for x in scores.tolist()],
            }
        )

    return {
        "succes": True,
        "meta": {
            "mode_unite": mode_unite,
            "nb_documents": len(documents),
            "nb_unites": len(unites),
            "nb_topics": nb_topics,
            "nb_mots_par_topic": nb_mots_par_topic,
            "vocabulaire_taille": len(noms_termes),
        },
        "topics": topics,
        "unites": detail_unites,
    }


def ecrire_json(chemin_sortie: str, donnees: Dict) -> None:
    """Écrit un dictionnaire JSON en UTF-8 lisible."""
    os.makedirs(os.path.dirname(os.path.abspath(chemin_sortie)), exist_ok=True)
    with open(chemin_sortie, "w", encoding="utf-8") as flux:
        json.dump(donnees, flux, ensure_ascii=False, indent=2)


def analyser_argumentaire() -> argparse.Namespace:
    """Construit et parse les arguments CLI."""
    parser = argparse.ArgumentParser(description="Analyse LDA à partir d'un JSON d'entrée.")
    parser.add_argument("--input", required=True, help="Chemin du JSON d'entrée.")
    parser.add_argument("--output", required=True, help="Chemin du JSON de sortie.")
    return parser.parse_args()


def main() -> int:
    """Point d'entrée principal du script."""
    args = analyser_argumentaire()

    try:
        donnees = charger_json(args.input)
        resultats = analyser_lda(donnees)
        ecrire_json(args.output, resultats)
        return 0
    except Exception as erreur:  # Gestion volontaire pour renvoyer une erreur propre à Shiny.
        details = {
            "succes": False,
            "erreur": str(erreur),
            "trace": traceback.format_exc(),
        }
        ecrire_json(args.output, details)
        print(f"Erreur pendant l'analyse LDA: {erreur}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
