#!/bin/bash

# Update README with SDL3 Binding Status
# ======================================
# This script generates binding status and inserts it into README.md

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "Updating README.md with current binding status..."

# Pull latest changes from current branch
echo "Pulling latest changes from current branch..."
git pull origin $(git branch --show-current)

# Extract SDL commit hash from CI workflow
SDL_COMMIT=""
if [[ -f ".github/workflows/deploy-readme.yml" ]]; then
    SDL_COMMIT=$(grep -o 'git checkout [a-f0-9]\{40\}' .github/workflows/deploy-readme.yml | cut -d' ' -f3)
fi

# Check if README.md exists
if [[ ! -f "README.md" ]]; then
    echo -e "${RED}Error: README.md not found${NC}"
    exit 1
fi

# Create backup
cp README.md README.md.backup
echo "Created backup: README.md.backup"

# Generate binding status
echo "Generating binding status..."
binding_status=$(bash .github/generate-binding-status.sh 2>/dev/null)

# Add SDL commit info to binding status if available
if [[ -n "$SDL_COMMIT" ]]; then
    # Insert SDL commit line after the date line
    binding_status=$(echo "$binding_status" | sed "/\*Last updated:/a\\
\\
*SDL3 commit: \`$SDL_COMMIT\`*")
fi

if [[ -z "$binding_status" ]]; then
    echo -e "${RED}Error: Failed to generate binding status${NC}"
    exit 1
fi

# Create temporary file for the new README
TEMP_README=$(mktemp)
trap "rm -f $TEMP_README" EXIT

# Check if binding status section already exists
if grep -q "## 📊 Binding Status" README.md; then
    echo "Updating existing binding status section..."

    # Extract content before binding status
    sed '/## 📊 Binding Status/,$d' README.md > "$TEMP_README"

    # Add new binding status
    echo "$binding_status" >> "$TEMP_README"

    # Find and add content after binding status (if any)
    # Look for the next ## section after binding status
    in_status=false
    found_next=false
    while IFS= read -r line; do
        if [[ "$line" == "## 📊 Binding Status" ]]; then
            in_status=true
            continue
        elif [[ "$in_status" == true && "$line" =~ ^##[[:space:]] ]]; then
            found_next=true
            echo "$line" >> "$TEMP_README"
        elif [[ "$found_next" == true ]]; then
            echo "$line" >> "$TEMP_README"
        fi
    done < README.md

else
    echo "Adding new binding status section..."

    # Find a good place to insert (after installation, before examples, or at end)
    if grep -q "^## " README.md; then
        # Find the first occurrence of ## Examples, ## Usage, ## Documentation, or ## Contributing
        insert_line=$(grep -n "^## \(Examples\|Usage\|Documentation\|Contributing\|License\)" README.md | head -1 | cut -d: -f1)

        if [[ -n "$insert_line" ]]; then
            # Insert before this section
            head -n $((insert_line - 1)) README.md > "$TEMP_README"
            echo "" >> "$TEMP_README"
            echo "$binding_status" >> "$TEMP_README"
            echo "" >> "$TEMP_README"
            tail -n +$insert_line README.md >> "$TEMP_README"
        else
            # Append at end
            cat README.md > "$TEMP_README"
            echo "" >> "$TEMP_README"
            echo "$binding_status" >> "$TEMP_README"
        fi
    else
        # No other sections, append at end
        cat README.md > "$TEMP_README"
        echo "" >> "$TEMP_README"
        echo "$binding_status" >> "$TEMP_README"
    fi
fi

# Replace original README with updated version
mv "$TEMP_README" README.md

echo -e "${GREEN}README.md updated successfully!${NC}"

# Show what changed
if command -v diff >/dev/null 2>&1; then
    echo -e "${BLUE}Changes made:${NC}"
    diff README.md.backup README.md || true
else
    echo "Updated README.md with binding status"
fi

# Optional: validate the README still looks good
if command -v wc >/dev/null 2>&1; then
    lines=$(wc -l < README.md)
    echo "Updated README.md has $lines lines"
fi

echo -e "${GREEN}README update complete!${NC}"
