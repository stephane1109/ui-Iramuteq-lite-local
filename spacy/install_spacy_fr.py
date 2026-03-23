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


def run_cmd(cmd: list[str], allow_fail: bool = False) -> bool:
    print("$", " ".join(cmd), flush=True)
    try:
        subprocess.check_call(cmd)
        return True
    except subprocess.CalledProcessError:
        if allow_fail:
            return False
        raise


def is_venv() -> bool:
    return (
        getattr(sys, "real_prefix", None) is not None
        or getattr(sys, "base_prefix", sys.prefix) != sys.prefix
    )


def pip_install(args: Iterable[str], repo_args: list[str] | None = None) -> None:
    base = [sys.executable, "-m", "pip", "install"]
    if not is_venv():
        # En environnement système, éviter les erreurs de droits.
        base.append("--user")
    if repo_args:
        base.extend(repo_args)
    run_cmd(base + list(args))


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description="Installe spaCy et un modèle français.")
    p.add_argument("--model", default="fr_core_news_lg", help="Nom du modèle spaCy à installer.")
    p.add_argument("--index-url", default=None, help="Dépôt Python principal (ex: https://pypi.org/simple).")
    p.add_argument("--extra-index-url", action="append", default=None, help="Dépôt Python secondaire (option répétable).")
    p.add_argument("--trusted-host", action="append", default=None, help="Hôte de confiance pip (option répétable).")
    return p.parse_args()


def main() -> int:
    args = parse_args()
    py = sys.executable
    repo_args: list[str] = []
    if args.index_url:
        repo_args.extend(["--index-url", args.index_url])
    if args.extra_index_url:
        for url in args.extra_index_url:
            repo_args.extend(["--extra-index-url", url])
    if args.trusted_host:
        for host in args.trusted_host:
            repo_args.extend(["--trusted-host", host])

    # 1) Installe spaCy côté Python (sans upgrade forcé de pip).
    pip_install(["spacy>=3.7,<4"], repo_args=repo_args)

    # 2) Installe le modèle FR via pip (compatible dépôts internes/miroirs).
    pip_model_name = args.model.replace("_", "-")
    model_cmd = [sys.executable, "-m", "pip", "install"]
    if not is_venv():
        model_cmd.append("--user")
    model_cmd.extend(repo_args)
    model_cmd.append(pip_model_name)
    model_ok = run_cmd(model_cmd, allow_fail=True)

    # 2b) Fallback: `spacy download` (utile si le package modèle n'est pas publié dans le dépôt pip ciblé).
    if not model_ok:
        run_cmd([py, "-m", "spacy", "download", args.model], allow_fail=False)

    # 3) Vérification rapide.
    run_cmd([py, "-m", "spacy", "info", args.model])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
