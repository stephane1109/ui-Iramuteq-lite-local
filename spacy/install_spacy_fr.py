#!/usr/bin/env python3
"""Installe spaCy + modèle FR via wheel URL (GitHub) ou paquet pip."""

from __future__ import annotations

import argparse
import subprocess
import sys
from typing import Iterable

SPACY_VERSION = "3.7.2"
DEFAULT_MODEL_WHEELS = {
    "fr_core_news_sm": "https://github.com/explosion/spacy-models/releases/download/fr_core_news_sm-3.7.0/fr_core_news_sm-3.7.0-py3-none-any.whl",
    "fr_core_news_md": "https://github.com/explosion/spacy-models/releases/download/fr_core_news_md-3.7.0/fr_core_news_md-3.7.0-py3-none-any.whl",
    "fr_core_news_lg": "https://github.com/explosion/spacy-models/releases/download/fr_core_news_lg-3.7.0/fr_core_news_lg-3.7.0-py3-none-any.whl",
}


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
    base = [sys.executable, "-m", "pip", "install", "--no-cache-dir"]
    if not is_venv():
        base.append("--user")
    if repo_args:
        base.extend(repo_args)
    run_cmd(base + list(args))


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description="Installe spaCy et un modèle français.")
    p.add_argument("--model", default="fr_core_news_lg", help="Nom du modèle spaCy à installer.")
    p.add_argument("--model-url", default=None, help="URL wheel directe du modèle (prioritaire).")
    p.add_argument("--repo-url", default=None, help="URL de dépôt pip pour spaCy / dépendances.")
    p.add_argument("--index-url", default=None, help="Alias de compatibilité pour --repo-url.")
    p.add_argument("--extra-index-url", action="append", default=None, help="Dépôt pip secondaire (répétable).")
    p.add_argument("--trusted-host", action="append", default=None, help="Hôte pip de confiance (répétable).")
    p.add_argument("--allow-download-fallback", action="store_true", help="Autorise `python -m spacy download` en dernier recours.")
    return p.parse_args()


def main() -> int:
    args = parse_args()
    repo_url = args.repo_url or args.index_url
    repo_args: list[str] = []
    if repo_url:
        repo_args.extend(["--index-url", repo_url])
    if args.extra_index_url:
        for url in args.extra_index_url:
            repo_args.extend(["--extra-index-url", url])
    if args.trusted_host:
        for host in args.trusted_host:
            repo_args.extend(["--trusted-host", host])

    # 1) spaCy + lookups (style Dockerfile fourni par l'utilisateur)
    pip_install([f"spacy=={SPACY_VERSION}", "spacy-lookups-data"], repo_args=repo_args)

    # 2) modèle FR via URL wheel prioritaire
    model_target = args.model_url or DEFAULT_MODEL_WHEELS.get(args.model) or args.model.replace("_", "-")
    model_ok = run_cmd([
        sys.executable,
        "-m",
        "pip",
        "install",
        "--no-cache-dir",
        *( ["--user"] if not is_venv() else [] ),
        *repo_args,
        model_target,
    ], allow_fail=True)

    if not model_ok:
        if not args.allow_download_fallback:
            print(
                "ERREUR: impossible d'installer le modèle. Fournissez --model-url vers un .whl valide "
                "ou activez --allow-download-fallback.",
                file=sys.stderr,
            )
            return 1
        run_cmd([sys.executable, "-m", "spacy", "download", args.model], allow_fail=False)

    # 3) vérification
    run_cmd([sys.executable, "-m", "spacy", "info", args.model])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
