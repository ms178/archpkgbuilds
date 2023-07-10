#!/bin/bash

# Iterate over subdirectories
for dir in */; do
  cd "$dir" || continue  # Move into the subdirectory

  # Check if PKGBUILD file exists
  if [[ -f PKGBUILD ]]; then
    pkgver=$(grep -oP "(?<=pkgver=)[^']+" PKGBUILD)  # Extract pkgver value

    # Check if pkgver ends with "23.04.1"
    if [[ $pkgver == *23.04.2 ]]; then
      # Extract the last digit and increment it
      last_digit=${pkgver: -1}
      new_last_digit=$((last_digit + 1))

      # Replace the last digit with the incremented value
      new_pkgver=${pkgver%?}$new_last_digit

      # Replace pkgver value in PKGBUILD
      sed -i "s/pkgver=$pkgver/pkgver=$new_pkgver/" PKGBUILD

      echo "Updated PKGBUILD in $dir"
    else
      echo "No update needed for PKGBUILD in $dir"
    fi
  fi

  cd ..  # Move back to the parent directory
done
