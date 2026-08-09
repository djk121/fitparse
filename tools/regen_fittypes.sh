#!/usr/bin/env bash
#
# Regenerate src/fittypes.rs from the current Garmin FIT profile.
#
# Fetches Profile.xlsx from the fit-sdk-tools GitHub releases, converts the
# Types/Messages sheets to the CSVs dump_fit_messages.py expects, and runs the
# generator. The profile files are build inputs and stay out of git.
#
# Usage:
#   tools/regen_fittypes.sh                 # latest FIT SDK release
#   tools/regen_fittypes.sh 21.212.0        # a specific release
#   OUT=/tmp/fittypes.rs tools/regen_fittypes.sh    # write elsewhere, e.g. to diff
#
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

venv="py-venv"
profile_dir="fit-profile"
out="${OUT:-src/fittypes.rs}"

if [ ! -d "$venv" ]; then
    echo "creating $venv ..."
    python3 -m venv "$venv"
fi

# inflect and jinja2 are dump_fit_messages.py's deps; openpyxl reads Profile.xlsx.
echo "installing generator dependencies ..."
"$venv/bin/pip" install --quiet --upgrade pip
"$venv/bin/pip" install --quiet inflect jinja2 openpyxl

fetch_args=()
if [ $# -gt 0 ]; then
    fetch_args+=(--version "$1")
fi
"$venv/bin/python" tools/fetch_fit_profile.py --out-dir "$profile_dir" "${fetch_args[@]}"

# Generate to a temp file first so a failure part-way through can't leave a
# truncated src/fittypes.rs behind.
tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT

echo "running dump_fit_messages.py ..."
"$venv/bin/python" tools/dump_fit_messages.py \
    "$profile_dir/Types.csv" "$profile_dir/Messages.csv" > "$tmp"

if [ ! -s "$tmp" ]; then
    echo "error: generator produced no output" >&2
    exit 1
fi

mkdir -p "$(dirname "$out")"
mv "$tmp" "$out"
trap - EXIT

echo "wrote $out ($(wc -l < "$out") lines) from FIT SDK $(cat "$profile_dir/VERSION")"
echo
echo "next: cargo build && cargo test"
