#!/usr/bin/env bash

set -euo pipefail

BINDING_SRC_DIR="src/SDL3"
SDL_HEADER_PATHS="SDL3/include/SDL3:/usr/local/include/SDL3:/usr/include/SDL3:/opt/homebrew/include/SDL3"

if ! command -v cabal >/dev/null 2>&1; then
  echo "Error: cabal not found in PATH." >&2
  exit 1
fi

echo "Building binding-checker..." >&2
cabal build exe:binding-checker -f -pkgconfig >/dev/null

# Always regenerate broken reports from a clean slate.
rm -rf broken
mkdir -p broken

sdl_dir=""
IFS=":" read -r -a header_paths <<< "${SDL_HEADER_PATHS}"
for path in "${header_paths[@]}"; do
  if [[ -d "${path}" ]]; then
    sdl_dir="${path}"
    break
  fi
done

if [[ -z "${sdl_dir}" ]]; then
  echo "Error: no SDL3 header directory found." >&2
  echo "Checked: ${SDL_HEADER_PATHS}" >&2
  exit 1
fi

modules=()
for file in "${BINDING_SRC_DIR}"/*.hs "${BINDING_SRC_DIR}"/*.hsc; do
  if [[ -f "${file}" ]]; then
    base="$(basename "${file}")"
    if [[ "${base}" == *.hs ]]; then
      modules+=("${base%.hs}")
    elif [[ "${base}" == *.hsc ]]; then
      modules+=("${base%.hsc}")
    fi
  fi
done

if [[ ${#modules[@]} -eq 0 ]]; then
  echo "Error: no binding modules found in ${BINDING_SRC_DIR}." >&2
  exit 1
fi

mapfile -t modules < <(printf "%s\n" "${modules[@]}" | sort -u)

tmp_dir="$(mktemp -d)"
trap 'rm -rf "${tmp_dir}"' EXIT
results_file="${tmp_dir}/results.tsv"

total_modules=0
modules_with_bindings=0
complete_bindings=0
missing_functions=0

echo "Checking ${#modules[@]} modules against headers in ${sdl_dir}..." >&2

for module in "${modules[@]}"; do
  module_lower="$(printf "%s" "${module}" | tr "[:upper:]" "[:lower:]")"
  header_name="SDL_${module_lower}.h"
  header_path="${sdl_dir}/${header_name}"
  module_label="SDL_${module_lower}"

  total_modules=$((total_modules + 1))

  if [[ ! -f "${header_path}" ]]; then
    printf "%s\t%s\n" "${module_label}" "❌ No header" >> "${results_file}"
    continue
  fi

  if ! output="$(printf "%s\n" "${header_path}" | cabal exec -f -pkgconfig binding-checker 2>&1)"; then
    echo "Error: binding-checker failed for ${header_path}" >&2
    echo "${output}" >&2
    exit 1
  fi

  if grep -q "No binding file found" <<< "${output}"; then
    printf "%s\t%s\n" "${module_label}" "❌ No bindings" >> "${results_file}"
  elif grep -q "✓ All bindings OK" <<< "${output}"; then
    printf "%s\t%s\n" "${module_label}" "✅ Complete" >> "${results_file}"
    complete_bindings=$((complete_bindings + 1))
    modules_with_bindings=$((modules_with_bindings + 1))
  elif grep -q "✗ Found .* broken bindings" <<< "${output}"; then
    count="$(grep -Eo "Found [0-9]+ broken bindings" <<< "${output}" | grep -Eo "[0-9]+" | head -n1 || true)"
    count="${count:-0}"
    printf "%s\t%s\n" "${module_label}" "⚠️ ${count} missing" >> "${results_file}"
    missing_functions=$((missing_functions + count))
    modules_with_bindings=$((modules_with_bindings + 1))
  else
    echo "Error: unexpected binding-checker output for ${header_path}" >&2
    echo "${output}" >&2
    exit 1
  fi
done

completion_rate=0
if [[ ${modules_with_bindings} -gt 0 ]]; then
  completion_rate=$((complete_bindings * 100 / modules_with_bindings))
fi

echo "## Binding Status"
echo
echo "### Summary"
echo "- **Total Modules**: ${total_modules}"
echo "- **Modules with Bindings**: ${modules_with_bindings}"
echo "- **Complete Bindings**: ${complete_bindings}"
echo "- **Missing Functions**: ${missing_functions}"
echo "- **Completion Rate**: ${completion_rate}%"
echo
echo "### Non-complete Modules"
if grep -qv $'\t✅ Complete' "${results_file}"; then
  echo
  echo "| Module | Status |"
  echo "| --- | --- |"
  while IFS=$'\t' read -r module status; do
    if [[ "${status}" != "✅ Complete" ]]; then
      echo "| \`${module}\` | ${status} |"
    fi
  done < <(sort "${results_file}")
  echo
else
  echo "- None"
  echo
fi

echo "### Notes"
echo "- Generated from \`${BINDING_SRC_DIR}\` against headers in \`${sdl_dir}\`."
echo "- Missing function details are written by \`binding-checker\` to \`broken/\`."

echo "Binding status generated (${complete_bindings}/${modules_with_bindings} complete)." >&2
