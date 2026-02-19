#!/usr/bin/env bash

set -euo pipefail

EXPECTED_COMMIT="a962f40bbba175e9716557a25d5d7965f134a3d3"
EXPECTED_MAJOR=3
EXPECTED_MINOR=4
EXPECTED_MICRO=0
HEADER_FILE="SDL3/include/SDL3/SDL_version.h"

if [[ ! -f "${HEADER_FILE}" ]]; then
  echo "Error: missing ${HEADER_FILE}" >&2
  exit 1
fi

if [[ ! -d SDL3/.git ]] && [[ ! -f SDL3/.git ]]; then
  echo "Error: SDL3 submodule is not initialized. Run: git submodule update --init --recursive" >&2
  exit 1
fi

read -r major minor micro < <(
  awk '
    /#define SDL_MAJOR_VERSION/ {maj=$3}
    /#define SDL_MINOR_VERSION/ {min=$3}
    /#define SDL_MICRO_VERSION/ {mic=$3}
    END {print maj, min, mic}
  ' "${HEADER_FILE}"
)

if [[ "${major}" != "${EXPECTED_MAJOR}" || "${minor}" != "${EXPECTED_MINOR}" || "${micro}" != "${EXPECTED_MICRO}" ]]; then
  echo "Error: SDL header version mismatch. Expected ${EXPECTED_MAJOR}.${EXPECTED_MINOR}.${EXPECTED_MICRO}, got ${major}.${minor}.${micro}." >&2
  exit 1
fi

actual_commit="$(git -C SDL3 rev-parse HEAD)"
if [[ "${actual_commit}" != "${EXPECTED_COMMIT}" ]]; then
  echo "Error: SDL3 submodule commit mismatch." >&2
  echo "Expected: ${EXPECTED_COMMIT}" >&2
  echo "Actual:   ${actual_commit}" >&2
  exit 1
fi

echo "SDL version check OK: ${major}.${minor}.${micro} @ ${actual_commit}"
