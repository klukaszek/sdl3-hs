#!/bin/bash
# SDL3 Haskell Bindings - Central Configuration
# =============================================
# Source this file from other scripts to get consistent paths.
#
# Usage: source "$(dirname "$0")/config.sh"

# Module namespace and source directory
BINDING_MODULE_NAME="SDL3"
BINDING_SRC_DIR="src/${BINDING_MODULE_NAME}"

# SDL3 commit hash used for binding validation
# Update this when targeting a new SDL3 version
SDL_HEADER_COMMIT="e1a623f129e75ad532315852d656fb26c80382a6"

# Search paths for SDL3 headers (colon-separated)
# Includes local submodule path first, then system paths
SDL_HEADER_PATHS="SDL3/include/SDL3:/usr/local/include/SDL3:/usr/include/SDL3:/opt/homebrew/include/SDL3"

# Export for subprocesses
export BINDING_MODULE_NAME BINDING_SRC_DIR SDL_HEADER_COMMIT SDL_HEADER_PATHS
