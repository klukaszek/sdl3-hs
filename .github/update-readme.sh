#!/usr/bin/env bash

set -euo pipefail

MODE="write"
if [[ "${1:-}" == "--check" ]]; then
  MODE="check"
elif [[ -n "${1:-}" ]]; then
  echo "Usage: $0 [--check]" >&2
  exit 2
fi

README_FILE="README.md"
BEGIN_MARKER="<!-- BEGIN_BINDING_STATUS -->"
END_MARKER="<!-- END_BINDING_STATUS -->"

if [[ ! -f "${README_FILE}" ]]; then
  echo "Error: ${README_FILE} not found." >&2
  exit 1
fi

if ! grep -q "${BEGIN_MARKER}" "${README_FILE}" || ! grep -q "${END_MARKER}" "${README_FILE}"; then
  echo "Error: README is missing binding-status markers." >&2
  echo "Expected markers:" >&2
  echo "  ${BEGIN_MARKER}" >&2
  echo "  ${END_MARKER}" >&2
  exit 1
fi

tmp_dir="$(mktemp -d)"
trap 'rm -rf "${tmp_dir}"' EXIT
status_file="${tmp_dir}/binding-status.md"
updated_readme="${tmp_dir}/README.updated.md"

./.github/generate-binding-status.sh > "${status_file}"

awk -v status_file="${status_file}" -v begin="${BEGIN_MARKER}" -v end="${END_MARKER}" '
  $0 == begin {
    print
    print ""
    while ((getline line < status_file) > 0) {
      print line
    }
    close(status_file)
    print ""
    in_block = 1
    replaced = 1
    next
  }
  $0 == end {
    in_block = 0
    print
    next
  }
  !in_block { print }
  END {
    if (!replaced) {
      exit 2
    }
  }
' "${README_FILE}" > "${updated_readme}"

if [[ "${MODE}" == "check" ]]; then
  if cmp -s "${README_FILE}" "${updated_readme}"; then
    echo "README binding status is up to date."
    exit 0
  fi
  echo "README binding status is stale. Run ./.github/update-readme.sh" >&2
  diff -u "${README_FILE}" "${updated_readme}" || true
  exit 1
fi

mv "${updated_readme}" "${README_FILE}"
echo "README binding status updated."
