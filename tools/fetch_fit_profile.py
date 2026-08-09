#!/usr/bin/env python3
"""Fetch the Garmin FIT profile and convert it to the CSVs dump_fit_messages.py wants.

As of FIT SDK 21.194 Garmin stopped shipping the SDK zip from developer.garmin.com
and publishes Profile.xlsx as a release asset on github.com/garmin/fit-sdk-tools
instead. (In the repo tree itself the file is a Git LFS pointer, so the release
asset is what we download.)

The generated CSVs are build inputs, not source - they land in an ignored
directory and are not committed.

Usage:
    fetch_fit_profile.py [--version TAG] [--out-dir DIR] [--keep-xlsx]
"""

import argparse
import csv
import json
import sys
import urllib.error
import urllib.request
from pathlib import Path

REPO = "garmin/fit-sdk-tools"
LATEST_RELEASE_URL = f"https://api.github.com/repos/{REPO}/releases/latest"
ASSET_URL = "https://github.com/{repo}/releases/download/{tag}/Profile.xlsx"

# dump_fit_messages.py unpacks each row into a fixed number of names, so the
# column count is part of the contract. If Garmin adds a column these numbers
# have to change here *and* in the generator's row unpacking.
TYPES_COLUMNS = 5
MESSAGES_COLUMNS = 16


def resolve_latest_tag():
    req = urllib.request.Request(
        LATEST_RELEASE_URL, headers={"Accept": "application/vnd.github+json"}
    )
    try:
        with urllib.request.urlopen(req, timeout=60) as resp:
            return json.load(resp)["tag_name"]
    except urllib.error.HTTPError as e:
        if e.code == 403:
            sys.exit(
                "error: GitHub API rate limit hit while resolving the latest "
                "release. Re-run with --version TAG to skip the lookup."
            )
        raise


def download(url, dest):
    try:
        with urllib.request.urlopen(url, timeout=120) as resp:
            data = resp.read()
    except urllib.error.HTTPError as e:
        sys.exit(f"error: failed to download {url}: HTTP {e.code}")

    # A Git LFS pointer is a small text file; getting one back means we asked
    # for the repo copy rather than the release asset.
    if data[:4] != b"PK\x03\x04":
        sys.exit(
            f"error: {url} did not return an xlsx file (got {len(data)} bytes "
            f"starting {data[:20]!r})"
        )
    dest.write_bytes(data)
    return len(data)


def sheet_to_csv(worksheet, dest, expected_columns, sheet_name):
    """Write a worksheet out as CSV with exactly expected_columns columns.

    Trailing columns beyond the ones the generator reads are dropped, but only
    after checking they are empty - silently discarding real spec data would
    produce a subtly wrong parser rather than an obvious failure.
    """
    rows = []
    for row_number, row in enumerate(worksheet.iter_rows(values_only=True), start=1):
        cells = ["" if c is None else str(c).strip() for c in row]

        overflow = cells[expected_columns:]
        if any(c for c in overflow):
            sys.exit(
                f"error: {sheet_name} row {row_number} has data in column "
                f"{expected_columns + 1} or beyond ({overflow!r}).\n"
                f"The FIT profile layout changed; dump_fit_messages.py's row "
                f"unpacking and {sheet_name.upper()}_COLUMNS both need updating."
            )

        cells = cells[:expected_columns]
        cells += [""] * (expected_columns - len(cells))
        rows.append(cells)

    with open(dest, "w", newline="", encoding="utf-8") as f:
        csv.writer(f).writerows(rows)
    return len(rows)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--version",
        metavar="TAG",
        help="FIT SDK release tag (e.g. 21.212.0); default: latest release",
    )
    parser.add_argument(
        "--out-dir",
        default="fit-profile",
        type=Path,
        help="where to write Types.csv/Messages.csv (default: fit-profile)",
    )
    parser.add_argument(
        "--keep-xlsx", action="store_true", help="keep the downloaded Profile.xlsx"
    )
    args = parser.parse_args()

    try:
        import openpyxl
    except ImportError:
        sys.exit(
            "error: openpyxl is required to read Profile.xlsx.\n"
            "  pip install openpyxl   (or run tools/regen_fittypes.sh, which "
            "sets up py-venv/ for you)"
        )

    tag = args.version or resolve_latest_tag()
    args.out_dir.mkdir(parents=True, exist_ok=True)
    xlsx_path = args.out_dir / "Profile.xlsx"

    url = ASSET_URL.format(repo=REPO, tag=tag)
    size = download(url, xlsx_path)
    print(f"downloaded Profile.xlsx from FIT SDK {tag} ({size} bytes)")

    workbook = openpyxl.load_workbook(xlsx_path, read_only=True, data_only=True)
    for required in ("Types", "Messages"):
        if required not in workbook.sheetnames:
            sys.exit(
                f"error: Profile.xlsx has no {required!r} sheet "
                f"(found {workbook.sheetnames})"
            )

    types_rows = sheet_to_csv(
        workbook["Types"], args.out_dir / "Types.csv", TYPES_COLUMNS, "Types"
    )
    messages_rows = sheet_to_csv(
        workbook["Messages"], args.out_dir / "Messages.csv", MESSAGES_COLUMNS, "Messages"
    )
    workbook.close()

    (args.out_dir / "VERSION").write_text(tag + "\n")
    if not args.keep_xlsx:
        xlsx_path.unlink()

    print(f"wrote {args.out_dir / 'Types.csv'} ({types_rows} rows)")
    print(f"wrote {args.out_dir / 'Messages.csv'} ({messages_rows} rows)")


if __name__ == "__main__":
    main()
