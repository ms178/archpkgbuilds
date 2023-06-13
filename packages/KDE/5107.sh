#!/bin/bash

# Set the target version and replacement version
target_version="5.106.1"
replacement_version="5.107.0"

# Find all PKGBUILD files recursively in the current directory
pkgbuild_files=$(find . -name PKGBUILD)

# Loop through each PKGBUILD file
for pkgbuild in $pkgbuild_files; do
    # Read the current pkgver from the PKGBUILD
    current_version=$(grep -E '^pkgver=' "$pkgbuild" | cut -d '=' -f 2)

    # Check if the current version matches the target version
    if [[ $current_version == $target_version ]]; then
        echo "Updating $pkgbuild from $current_version to $replacement_version"

        # Replace the pkgver with the new version
        sed -i "s/^pkgver=.*/pkgver=$replacement_version/" "$pkgbuild"
    fi
done
