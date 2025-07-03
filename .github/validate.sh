#!/bin/bash

# SDL3 Binding Checker Validation Script
# ======================================
# This script validates the entire SDL3 binding system including:
# - GitHub workflow components
# - SDL3 installation and headers
# - Binding checker functionality
# - Status generation and README updates
# - Pandoc template processing

# Don't use set -e here as we want to handle errors gracefully

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
NC='\033[0m'

# Test mode: quick or full
MODE="${1:-quick}"

# Handle help
if [[ "$MODE" == "--help" || "$MODE" == "-h" ]]; then
    echo "SDL3 Binding Validation Script"
    echo "=============================="
    echo ""
    echo "Usage:"
    echo "  $0              Quick validation (essential checks only)"
    echo "  $0 quick        Same as above"
    echo "  $0 full         Comprehensive validation with functionality tests"
    echo "  $0 --help       Show this help"
    echo ""
    echo "Quick mode checks:"
    echo "  - Essential files and permissions"
    echo "  - Build system (cabal, binding checker)"
    echo "  - SDL3 installation and headers"
    echo "  - Configuration files"
    echo "  - GitHub workflow structure"
    echo ""
    echo "Full mode adds:"
    echo "  - Actual binding status generation"
    echo "  - README update functionality"
    echo "  - SDL3 compilation test"
    echo "  - Binding checker execution"
    echo "  - Pandoc HTML generation"
    echo "  - Git ignore functionality"
    echo ""
    echo "Examples:"
    echo "  $0              # Quick validation before commit"
    echo "  $0 full         # Comprehensive testing before deploy"
    exit 0
fi

echo -e "${BLUE}🔍 SDL3 Binding Validation${NC}"
echo "=========================="
echo "Mode: $MODE"
echo ""

errors=0
warnings=0

test_result() {
    local name="$1"
    local level="$2"
    local message="$3"

    case "$level" in
        "PASS")
            echo -e "${GREEN}✅ $name${NC}"
            [[ -n "$message" ]] && echo -e "   $message"
            ;;
        "FAIL")
            echo -e "${RED}❌ $name${NC}"
            [[ -n "$message" ]] && echo -e "   ${RED}$message${NC}"
            errors=$((errors + 1))
            ;;
        "WARN")
            echo -e "${YELLOW}⚠️  $name${NC}"
            [[ -n "$message" ]] && echo -e "   ${YELLOW}$message${NC}"
            warnings=$((warnings + 1))
            ;;
        *)
            echo -e "${BLUE}ℹ️  $name${NC}"
            [[ -n "$message" ]] && echo -e "   $message"
            ;;
    esac
}

# Essential Files Check
echo -e "${PURPLE}📁 Essential Files${NC}"
echo "-------------------"

required_files=(
    ".github/workflows/deploy-readme.yml"
    ".github/template.html"
    ".github/generate-binding-status.sh"
    ".github/update-readme.sh"
    ".github/INTEGRATION.md"
    "check-sdl-bindings"
    "test/binding-checker.hs"
    "sdl3.cabal"
    "README.md"
    ".gitignore"
)

for file in "${required_files[@]}"; do
    if [[ -f "$file" ]]; then
        test_result "$file" "PASS"
    else
        test_result "$file" "FAIL" "Required file missing"
    fi
done

echo ""

# Permissions Check
echo -e "${PURPLE}🔐 Permissions${NC}"
echo "---------------"

executable_files=(
    ".github/generate-binding-status.sh"
    ".github/update-readme.sh"
    "check-sdl-bindings"
)

for file in "${executable_files[@]}"; do
    if [[ -x "$file" ]]; then
        test_result "$file executable" "PASS"
    else
        test_result "$file executable" "FAIL" "Missing executable permission"
    fi
done

echo ""

# Build System Check
echo -e "${PURPLE}📦 Build System${NC}"
echo "----------------"

if command -v cabal >/dev/null 2>&1; then
    test_result "Cabal available" "PASS"

    if cabal build exe:binding-checker -f-pkgconfig >/dev/null 2>&1; then
        test_result "Binding checker builds" "PASS"
    else
        test_result "Binding checker builds" "FAIL" "Cannot build binding checker"
    fi

    if cabal check >/dev/null 2>&1; then
        test_result "Cabal file validation" "PASS"
    else
        test_result "Cabal file validation" "WARN" "Cabal check reported packaging warnings"
    fi
else
    test_result "Cabal available" "FAIL" "Cabal not found"
fi

echo ""

# SDL3 Integration Check
echo -e "${PURPLE}🎮 SDL3 Integration${NC}"
echo "--------------------"

if pkg-config --exists sdl3 2>/dev/null; then
    version=$(pkg-config --modversion sdl3)
    test_result "SDL3 pkg-config" "PASS" "Version: $version"
else
    test_result "SDL3 pkg-config" "FAIL" "SDL3 not found via pkg-config"
fi

# Check for SDL3 headers
sdl3_found=false
for path in "/usr/local/include/SDL3" "/usr/include/SDL3" "/opt/homebrew/include/SDL3"; do
    if [[ -d "$path" && -f "$path/SDL.h" ]]; then
        test_result "SDL3 headers" "PASS" "Found in $path"
        sdl3_found=true
        break
    fi
done

if [[ "$sdl3_found" == false ]]; then
    test_result "SDL3 headers" "FAIL" "No SDL3 headers found"
fi

echo ""

# Configuration Check
echo -e "${PURPLE}📝 Configuration${NC}"
echo "-----------------"

if grep -F '$title$' .github/template.html >/dev/null && grep -F '$body$' .github/template.html >/dev/null; then
    test_result "Pandoc template variables" "PASS"
else
    test_result "Pandoc template variables" "FAIL" "Missing required template variables"
fi

if grep -q "broken/" .gitignore && grep -q "sdl/" .gitignore; then
    test_result "Git ignore rules" "PASS"
else
    test_result "Git ignore rules" "FAIL" "Missing ignore rules for generated directories"
fi

echo ""

if [[ "$MODE" == "full" ]]; then
    # Full Functionality Tests
    echo -e "${PURPLE}🔧 Full Functionality Tests${NC}"
    echo "----------------------------"

    # Test binding status generation
    if ./.github/generate-binding-status.sh >/dev/null 2>&1; then
        test_result "Binding status generation" "PASS"
    else
        test_result "Binding status generation" "FAIL" "Status generation failed"
    fi

    # Test README update
    if [[ -f "README.md" ]]; then
        cp README.md README.md.test-backup

        if ./.github/update-readme.sh >/dev/null 2>&1; then
            test_result "README update" "PASS"

            if grep -q "📊 Binding Status" README.md; then
                test_result "README status section" "PASS"
            else
                test_result "README status section" "FAIL" "Status section missing"
            fi
        else
            test_result "README update" "FAIL" "README update failed"
        fi

        # Restore README
        mv README.md.test-backup README.md
    fi

    # Test SDL3 compilation
    if pkg-config --exists sdl3 2>/dev/null; then
        temp_c=$(mktemp)
        mv "$temp_c" "$temp_c.c"
        temp_c="$temp_c.c"
        temp_exe=$(mktemp)

        cat > "$temp_c" << 'EOF'
#include <SDL3/SDL.h>
int main() {
    if (SDL_Init(SDL_INIT_VIDEO) < 0) {
        return 1;
    }
    SDL_Quit();
    return 0;
}
EOF

        if gcc $(pkg-config --cflags sdl3) -o "$temp_exe" "$temp_c" $(pkg-config --libs sdl3) 2>/dev/null; then
            test_result "SDL3 compile test" "PASS"
        else
            test_result "SDL3 compile test" "FAIL" "Cannot compile against SDL3"
        fi

        rm -f "$temp_c" "$temp_exe"
    fi

    # Test binding checker execution
    test_header=""
    for path in "/usr/local/include/SDL3" "/usr/include/SDL3" "/opt/homebrew/include/SDL3"; do
        if [[ -f "$path/SDL_init.h" ]]; then
            test_header="$path/SDL_init.h"
            break
        fi
    done

    if [[ -n "$test_header" ]]; then
        if echo "$test_header" | cabal exec binding-checker >/dev/null 2>&1; then
            test_result "Binding checker execution" "PASS"
        else
            test_result "Binding checker execution" "FAIL" "Binding checker failed"
        fi
    else
        test_result "Binding checker test" "WARN" "No test header found"
    fi

    # Test Pandoc processing
    if command -v pandoc >/dev/null 2>&1; then
        temp_html=$(mktemp)
        mv "$temp_html" "$temp_html.html"
        temp_html="$temp_html.html"

        if pandoc --standalone \
                 -f gfm \
                 -t html5 \
                 --template=.github/template.html \
                 --metadata title="Test" \
                 -o "$temp_html" \
                 README.md 2>/dev/null; then
            test_result "Pandoc HTML generation" "PASS"

            if [[ -f "$temp_html" ]] && [[ $(wc -c < "$temp_html") -gt 1000 ]]; then
                test_result "HTML output size" "PASS"
            else
                test_result "HTML output size" "WARN" "Generated HTML seems small"
            fi
        else
            test_result "Pandoc HTML generation" "FAIL" "HTML generation failed"
        fi

        rm -f "$temp_html"
    else
        test_result "Pandoc availability" "WARN" "Pandoc not available"
    fi

    # Test git ignore functionality
    mkdir -p broken sdl test-temp

    if git status --porcelain | grep -E "(broken/|sdl/|test-temp/)" >/dev/null 2>&1; then
        test_result "Git ignore functionality" "FAIL" "Directories not ignored"
    else
        test_result "Git ignore functionality" "PASS"
    fi

    rm -rf broken sdl test-temp

    echo ""
fi

# GitHub Workflow Validation
echo -e "${PURPLE}⚙️  GitHub Workflow${NC}"
echo "-------------------"

workflow_file=".github/workflows/deploy-readme.yml"
if [[ -f "$workflow_file" ]]; then
    test_result "Workflow file exists" "PASS"

    if grep -q "on:" "$workflow_file" && grep -q "jobs:" "$workflow_file"; then
        test_result "Workflow structure" "PASS"
    else
        test_result "Workflow structure" "FAIL" "Invalid workflow structure"
    fi

    # Check for SDL3 header setup (either full compilation or header cloning)
    if grep -q "cmake" "$workflow_file" && grep -q "pkg-config.*sdl3" "$workflow_file"; then
        test_result "SDL3 installation steps" "PASS" "Full SDL3 compilation setup"
    elif grep -q "Clone SDL3 headers" "$workflow_file" || grep -q "git clone.*SDL" "$workflow_file"; then
        test_result "SDL3 installation steps" "PASS" "SDL3 header cloning setup"
    elif grep -q "libsdl3-dev" "$workflow_file"; then
        test_result "SDL3 installation steps" "WARN" "Package manager SDL3 installation"
    else
        test_result "SDL3 installation steps" "FAIL" "No SDL3 installation found"
    fi

    # Check for proper error handling (no || echo "Warning")
    if grep -q "|| echo.*Warning" "$workflow_file"; then
        test_result "Error handling" "FAIL" "Workflow masks errors with warnings"
    else
        test_result "Error handling" "PASS"
    fi
else
    test_result "Workflow file exists" "FAIL"
fi

echo ""

# Summary
echo -e "${BLUE}📊 Validation Summary${NC}"
echo "===================="
echo "Total Issues: $((errors + warnings))"
echo -e "${RED}Errors: $errors${NC}"
echo -e "${YELLOW}Warnings: $warnings${NC}"
echo ""

if [[ $errors -eq 0 ]]; then
    echo -e "${GREEN}🎉 Validation passed!${NC}"
    if [[ "$MODE" == "full" ]]; then
        echo -e "${GREEN}All functionality tests completed successfully.${NC}"
    else
        echo -e "${GREEN}Essential components validated. Run './github/validate.sh full' for comprehensive testing.${NC}"
    fi

    if [[ $warnings -gt 0 ]]; then
        echo -e "${YELLOW}⚠️  Some warnings found - review for potential improvements.${NC}"
    fi

    exit 0
else
    echo -e "${RED}❌ Validation failed!${NC}"
    echo -e "${RED}Fix the errors above before deploying.${NC}"
    exit 1
fi
