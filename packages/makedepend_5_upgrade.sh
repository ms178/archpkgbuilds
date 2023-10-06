#!/bin/bash

# Pfad zur Liste mit den Einträgen
liste="/home/marcus/Downloads/KDE/liste.txt"

# Pfad zum Verzeichnis mit den PKGBUILD-Dateien
verzeichnis="/home/marcus/Downloads/KDE"

# Durchlaufe die Einträge in der Liste
while IFS= read -r eintrag
do
  # Überprüfe, ob der Eintrag eine 5 enthält
  if [[ $eintrag == *5* ]]; then
    continue
  fi

  # Durchlaufe die Unterverzeichnisse im angegebenen Verzeichnis
  for pkgbuild in "$verzeichnis"/*/PKGBUILD
  do
    # Extrahiere den aktuellen pkgname aus der PKGBUILD-Datei
    aktueller_pkgname=$(awk -F "=" "/^pkgname=/ {print \$2}" "$pkgbuild")

    # Überprüfe, ob der aktuelle pkgname keine 5 enthält
    if [[ $aktueller_pkgname != *5* ]]; then
      # Aktualisiere die pkgname-Sektion in der PKGBUILD-Datei
      sed -i "s/pkgname=.*/pkgname=\('${eintrag}'-new)/" "$pkgbuild"
    fi
  done
done < "$liste"
