#!/usr/bin/env python3
"""Installe spaCy + le modèle français large fr_core_news_lg.

Usage:
  python spacy/install_spacy_fr.py
"""

from __future__ import annotations

import subprocess
import sys


def run_cmd(cmd: list[str]) -> None:
    print("$", " ".join(cmd), flush=True)
    subprocess.check_call(cmd)


def main() -> int:
    py = sys.executable

    # 1) Installe/maj spaCy côté Python.
    run_cmd([py, "-m", "pip", "install", "--upgrade", "pip"])
    run_cmd([py, "-m", "pip", "install", "--upgrade", "spacy"])

    # 2) Télécharge le modèle large FR demandé.
    run_cmd([py, "-m", "spacy", "download", "fr_core_news_lg"])

    # 3) Vérification rapide.
    code = (
        "import spacy; "
        "nlp=spacy.load('fr_core_news_lg'); "
        "print('OK spaCy:', spacy.__version__, '| model:', nlp.meta.get('name'))"
    )
    run_cmd([py, "-c", code])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
