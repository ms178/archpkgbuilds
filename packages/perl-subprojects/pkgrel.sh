#!/bin/bash

# Iterate over subdirectories
for dir in */; do
  cd "$dir" || continue  # Move into the subdirectory

  # Check if PKGBUILD file exists
  if [[ -f PKGBUILD ]]; then
    pkgrel=$(grep -oP "(?<=pkgrel=)[^']+" PKGBUILD)  # Extract pkgrel value

    # Check if pkgrel value has dot and 1
    if [[ $pkgrel =~ \.1$ ]]; then
      # Split the pkgrel value into two parts
      IFS='.' read -r -a parts <<< "$pkgrel"

      # Increment the first part
      new_pkgrel=$((parts[0] + 1))

      # Create the new_pkgrel value with dot and 1
      new_pkgrel="${new_pkgrel}.1"

      # Replace pkgrel value in PKGBUILD
      sed -i "s/pkgrel=$pkgrel/pkgrel=$new_pkgrel/" PKGBUILD
    else
      # Increment pkgrel value by 1
      new_pkgrel=$((pkgrel + 1))

      # Replace pkgrel value in PKGBUILD
      sed -i "s/pkgrel=$pkgrel/pkgrel=$new_pkgrel/" PKGBUILD
    fi

    echo "Updated PKGBUILD in $dir"
  fi

  cd ..  # Move back to the parent directory
done
