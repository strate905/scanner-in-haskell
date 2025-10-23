#!/usr/bin/env bash
set -euo pipefail

# Simple sanity check that the scanner builds and runs against localhost.
# Requires dependencies listed in scanner.cabal to be available in the cabal store.

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

echo "[1/2] Building scanner…"
cabal build

echo "[2/2] Running scanner against 127.0.0.1 (Markdown output)…"
cabal run scanner -- 127.0.0.1

echo "[2/2b] Running scanner against 127.0.0.1 (JSON output)…"
cabal run scanner -- --json 127.0.0.1

echo "Smoke test complete."
