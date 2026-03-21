#!/usr/bin/env python3
"""Parse POS tags with spaCy from a CSV input and write token-level CSV output."""

from __future__ import annotations

import argparse
import csv
import sys
from pathlib import Path


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="spaCy POS parser for IRaMuTeQ-lite")
    parser.add_argument("--input-csv", required=True, help="CSV input path with columns: doc_id,text")
    parser.add_argument("--output-csv", required=True, help="CSV output path with columns: doc_id,token,pos")
    parser.add_argument("--model", default="fr_core_news_sm", help="spaCy model name")
    return parser.parse_args()


def normalize_token(token: str) -> str:
    tok = (token or "").strip().lower()
    return tok


def main() -> int:
    args = parse_args()
    in_path = Path(args.input_csv)
    out_path = Path(args.output_csv)

    if not in_path.exists():
        print(f"Input CSV not found: {in_path}", file=sys.stderr)
        return 2

    try:
        import spacy  # type: ignore
    except Exception as exc:  # pragma: no cover - runtime dependency
        print(f"Python package 'spacy' unavailable: {exc}", file=sys.stderr)
        return 3

    try:
        nlp = spacy.load(args.model, disable=["ner", "textcat"])
    except Exception as exc:  # pragma: no cover - runtime dependency
        print(f"Unable to load spaCy model '{args.model}': {exc}", file=sys.stderr)
        return 4

    rows: list[tuple[str, str]] = []
    with in_path.open("r", encoding="utf-8", newline="") as f_in:
        reader = csv.DictReader(f_in)
        if reader.fieldnames is None or "doc_id" not in reader.fieldnames or "text" not in reader.fieldnames:
            print("Input CSV must contain headers: doc_id,text", file=sys.stderr)
            return 5
        for rec in reader:
            doc_id = (rec.get("doc_id") or "").strip()
            text = rec.get("text") or ""
            rows.append((doc_id, text))

    out_path.parent.mkdir(parents=True, exist_ok=True)
    with out_path.open("w", encoding="utf-8", newline="") as f_out:
        writer = csv.writer(f_out)
        writer.writerow(["doc_id", "token", "pos"])

        for doc_id, text in rows:
            doc = nlp(text)
            for tok in doc:
                token_norm = normalize_token(tok.text)
                if not token_norm:
                    continue
                pos = (tok.pos_ or "").strip().upper()
                if not pos:
                    continue
                writer.writerow([doc_id, token_norm, pos])

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
