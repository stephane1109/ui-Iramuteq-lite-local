#!/usr/bin/env python3
import argparse
import collections
import csv
import sys


def parse_args():
    p = argparse.ArgumentParser(description="Extract NER entities from text using spaCy.")
    p.add_argument("--input-txt", required=True, help="UTF-8 input text file.")
    p.add_argument("--output-csv", required=True, help="Output CSV path.")
    p.add_argument("--model", default="fr_core_news_lg", help="spaCy model name.")
    return p.parse_args()


def main():
    args = parse_args()

    try:
        import spacy
    except Exception as exc:
        print(f"spaCy import failed: {exc}", file=sys.stderr)
        return 2

    try:
        nlp = spacy.load(args.model)
    except Exception as exc:
        print(f"spaCy model load failed ({args.model}): {exc}", file=sys.stderr)
        return 3

    try:
        with open(args.input_txt, "r", encoding="utf-8") as f:
            text = f.read()
    except Exception as exc:
        print(f"Cannot read input text: {exc}", file=sys.stderr)
        return 4

    doc = nlp(text)
    by_key = collections.defaultdict(list)
    for ent in doc.ents:
        etxt = ent.text.strip()
        label = ent.label_.strip().upper()
        if not etxt or not label:
            continue
        key = (etxt.lower(), label)
        by_key[key].append(etxt)

    rows = []
    for (_, label), forms in by_key.items():
        counts = collections.Counter(forms)
        best_form, freq = counts.most_common(1)[0]
        rows.append((best_form, label, int(freq)))

    rows.sort(key=lambda x: (-x[2], x[0]))

    try:
        with open(args.output_csv, "w", encoding="utf-8", newline="") as f:
            writer = csv.writer(f)
            writer.writerow(["text", "label", "freq"])
            writer.writerows(rows)
    except Exception as exc:
        print(f"Cannot write output CSV: {exc}", file=sys.stderr)
        return 5

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
