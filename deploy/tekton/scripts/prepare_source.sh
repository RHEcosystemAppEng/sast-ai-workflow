#!/usr/bin/env sh
set -ex
echo "=== STEP 3: PREPARE SOURCE ==="

# Clean workspace and prepare directories
rm -rf "${WORKDIR:?}/source" 2>/dev/null || true
mkdir -p "${WORKDIR}/source"

if echo "$SRC_URL" | grep -iq '\.rpm$'; then
  echo "Processing SRPM package..."

  # Setup rpmbuild environment
  mkdir -p "$HOME"/rpmbuild/{BUILD,RPMS,SOURCES,SPECS,SRPMS}
  echo '%_topdir %(echo $HOME)/rpmbuild' > "$HOME/.rpmmacros"

  # Download and extract SRPM
  curl -ksLf "$SRC_URL" -o "$HOME/package.src.rpm"
  rpm -ivh "$HOME/package.src.rpm" >/dev/null 2>&1

  # Build source
  SPEC="$(find "$HOME/rpmbuild/SPECS" -name '*.spec' | head -n1)"
  if [ -z "$SPEC" ]; then
    echo "Error: No .spec found in SRPM" >&2
    exit 1
  fi
  rpmbuild -bp "$SPEC" >/dev/null 2>&1

  # Find and copy source directory
  BUILD_DIR=$(find "$HOME/rpmbuild/BUILD" -maxdepth 1 -type d ! -path "$HOME/rpmbuild/BUILD" | head -n1)
  if [ -z "$BUILD_DIR" ]; then
    echo "Error: No source directory found in BUILD after rpmbuild" >&2
    exit 1
  fi

  SOURCE_DIR_NAME=$(basename "$BUILD_DIR")
  cp -r "$BUILD_DIR" "${WORKDIR}/source/"
  REPO_LOCAL_PATH="${WORKDIR}/source/$SOURCE_DIR_NAME"

  # Cleanup
  rm -rf "$HOME/rpmbuild" "$HOME/package.src.rpm" "$HOME/.rpmmacros"

else
  echo "Processing Git repository..."

  REPO_NAME=$(basename "$SRC_URL" .git)
  git clone "$SRC_URL" "${WORKDIR}/source/$REPO_NAME" >/dev/null 2>&1
  REPO_LOCAL_PATH="${WORKDIR}/source/$REPO_NAME"
fi

# Verify source preparation
if [ ! -d "$REPO_LOCAL_PATH" ]; then
  echo "Error: Repository directory not found: $REPO_LOCAL_PATH" >&2
  exit 1
fi

# Save repo path for next steps
echo -n "$REPO_LOCAL_PATH" > "${TEKTON_RESULTS_DIR}"
echo "REPO_LOCAL_PATH=$REPO_LOCAL_PATH" > /shared-data/env.txt
echo "Source code prepared successfully"