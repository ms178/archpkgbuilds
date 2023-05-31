#!/bin/bash
# Find all PKGBUILD files in subdirectories
find ./ -name PKGBUILD -print0 | while read -d '' -r file;
do
  # Get the current value of pkgrel
  pkgrel=$(awk -F= '/^pkgrel/ {print $2}' "$file" | tr -d ' ')
  # Increment pkgrel
  new_pkgrel=$(echo "$pkgrel + 1" | bc)
  # Get the current value of pkgver
  pkgver=$(awk -F= '/^pkgver/ {print $2}' "$file" | tr -d ' ')
  # Build the new version string
  new_version="$new_pkgrel.1"
  # Replace the old pkgrel with the new one
  sed -i "s/^pkgrel=.*/pkgrel=$new_pkgrel/" "$file"
  # Replace the old pkgver with the new one, including the new version string
  sed -i "s/^pkgver=.*/pkgver=${pkgver}_${new_version}/" "$file"
done
