#!/usr/bin/env python3
"""Installe spaCy + le modèle français large fr_core_news_lg.

Usage:
  python spacy/install_spacy_fr.py
"""

from __future__ import annotations

import argparse
import subprocess
import sys
from typing import Iterable


def run_cmd(cmd: list[str]) -> None:
    print("$", " ".join(cmd), flush=True)
    subprocess.check_call(cmd)


def is_venv() -> bool:
    return (
        getattr(sys, "real_prefix", None) is not None
        or getattr(sys, "base_prefix", sys.prefix) != sys.prefix
    )


def pip_install(args: Iterable[str]) -> None:
    base = [sys.executable, "-m", "pip", "install"]
    if not is_venv():
        # En environnement système, éviter les erreurs de droits.
        base.append("--user")
    run_cmd(base + list(args))


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description="Installe spaCy et un modèle français.")
    p.add_argument("--model", default="fr_core_news_lg", help="Nom du modèle spaCy à installer.")
    return p.parse_args()


def main() -> int:
    args = parse_args()
    py = sys.executable

    # 1) Installe spaCy côté Python (sans upgrade forcé de pip).
    pip_install(["spacy>=3.7,<4"])

    # 2) Télécharge le modèle large FR demandé.
    run_cmd([py, "-m", "spacy", "download", args.model])

    # 3) Vérification rapide.
    code = (
        "import spacy; "
        f"nlp=spacy.load('{args.model}'); "
        "print('OK spaCy:', spacy.__version__, '| model:', nlp.meta.get('name'))"
    )
    run_cmd([py, "-c", code])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
