#!/bin/bash

# Generate SDL3 Binding Status for README
# =======================================
# This script runs the binding checker and generates a status table
# to be included in the README.md file.

set -e

# Source central configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/config.sh"

echo "Generating SDL3 binding status..." >&2

# Check if cabal and binding-checker are available
if ! command -v cabal >/dev/null 2>&1; then
    echo "Error: cabal not found. Cannot generate binding status." >&2
    exit 1
fi

# Build the binding checker
echo "Building binding checker..." >&2
cabal build exe:binding-checker -f-pkgconfig >/dev/null 2>&1

# Find SDL3 headers (check submodule first, then system paths)
SDL_PATHS="SDL3/include/SDL3:/usr/local/include/SDL3:/usr/include/SDL3:/opt/homebrew/include/SDL3"
sdl_dir=""

IFS=':' read -ra PATHS <<< "$SDL_PATHS"
for path in "${PATHS[@]}"; do
    if [[ -d "$path" ]]; then
        sdl_dir="$path"
        break
    fi
done

if [[ -z "$sdl_dir" ]]; then
    echo "Error: No SDL3 headers found in standard locations" >&2
    echo "Please install SDL3 development files first." >&2
    echo "Searched paths: $SDL_PATHS" >&2
    exit 1
fi

# Create temporary directory for results
TEMP_DIR=$(mktemp -d)
trap "rm -rf $TEMP_DIR" EXIT

# Get list of modules from source directory
modules=()
for file in ${BINDING_SRC_DIR}/*.hsc ${BINDING_SRC_DIR}/*.hs; do
    if [[ -f "$file" ]]; then
        basename=$(basename "$file" .hsc)
        basename=$(basename "$basename" .hs)
        modules+=("$basename")
    fi
done

# Check if we found any modules
if [[ ${#modules[@]} -eq 0 ]]; then
    echo "Error: No Haskell binding modules found in ${BINDING_SRC_DIR}/" >&2
    exit 1
fi

# Check each module and collect results
echo "Checking bindings for ${#modules[@]} modules..." >&2

results_file="$TEMP_DIR/results.txt"
total_headers=0
total_broken=0
headers_with_bindings=0
headers_complete=0

for module in "${modules[@]}"; do
    # Convert module name to header name (e.g., "Camera" -> "SDL_camera.h")
    module_lower=$(echo "$module" | tr '[:upper:]' '[:lower:]')
    header_name="SDL_${module_lower}.h"
    header_path="$sdl_dir/$header_name"

    total_headers=$((total_headers + 1))

    # Run binding checker on individual header
    if [[ -f "$header_path" ]]; then
        if ! output=$(echo "$header_path" | cabal exec -f-pkgconfig binding-checker 2>&1); then
            # If binding checker fails, it's a critical error
            echo "Error: Binding checker failed for $header_path" >&2
            echo "Output: $output" >&2
            exit 1
        fi
    else
        # Header doesn't exist, assume no bindings needed
        echo "SDL_${module_lower}:❌ No header" >> "$results_file"
        continue
    fi

    if echo "$output" | grep -q "No binding file found"; then
        echo "SDL_${module_lower}:❌ No bindings" >> "$results_file"
    elif echo "$output" | grep -q "✓ All bindings OK"; then
        echo "SDL_${module_lower}:✅ Complete" >> "$results_file"
        headers_complete=$((headers_complete + 1))
        headers_with_bindings=$((headers_with_bindings + 1))
    elif echo "$output" | grep -q "✗ Found.*broken bindings"; then
        count=$(echo "$output" | grep -o "Found [0-9]* broken bindings" | grep -o "[0-9]*" || echo "0")
        echo "SDL_${module_lower}:⚠️ $count missing" >> "$results_file"
        total_broken=$((total_broken + count))
        headers_with_bindings=$((headers_with_bindings + 1))
    else
        echo "SDL_${module_lower}:❓ Unknown" >> "$results_file"
    fi
done

# Calculate statistics
completion_percentage=0
if [[ $headers_with_bindings -gt 0 ]]; then
    completion_percentage=$(( (headers_complete * 100) / headers_with_bindings ))
fi

# Generate timestamp
timestamp=$(date -u +"%Y-%m-%d %H:%M UTC")

# Generate the binding status section
echo "## 📊 Binding Status"
echo ""
echo "*Last updated: $timestamp*"
echo ""
echo "### Summary"
echo "- **Total Modules**: $total_headers"
echo "- **Modules with Bindings**: $headers_with_bindings"
echo "- **Complete Bindings**: $headers_complete"
echo "- **Missing Functions**: $total_broken"
echo "- **Completion Rate**: ${completion_percentage}%"
echo ""
echo "### Status by Module"
echo ""
echo "| Module | Status |"
echo "|--------|--------|"

# Sort results and create simple two-column table
sorted_results=$(sort "$results_file")

while IFS= read -r line; do
    module=$(echo "$line" | cut -d: -f1)
    status=$(echo "$line" | cut -d: -f2-)
    echo "| \`$module\` | $status |"
done <<< "$sorted_results"

echo ""
echo "### Legend"
echo "- ✅ **Complete**: All functions from the header are bound"
echo "- ⚠️ **X missing**: Header has bindings but X functions are missing"
echo "- ❌ **No bindings**: No Haskell bindings exist for this header"
echo "- ❓ **Unknown**: Status could not be determined"
echo ""
echo "### Notes"
echo "- Status reflects core SDL3 headers (test/internal headers excluded)"
echo "- Missing function details are available in the \`broken/\` directory after running the binding checker"
echo "- Use \`./check-sdl-bindings -i\` for interactive binding status checking"
echo "- Some modules may intentionally have no bindings if not applicable to Haskell"
echo ""

echo "Binding status generated successfully! Total: $total_headers modules, $headers_complete complete, $total_broken missing functions" >&2
