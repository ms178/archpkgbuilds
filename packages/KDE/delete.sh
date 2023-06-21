#!/bin/bash

# Function to delete files matching the pattern recursively
delete_files() {
    local dir="$1"

    # Loop through files and directories in the given directory
    for file in "$dir"/*; do
        if [[ -f "$file" && "$file" == *".tar"* ]]; then
            # Delete the file if it matches the pattern
            echo "Deleting file: $file"
            rm "$file"
        elif [[ -d "$file" ]]; then
            # Recursively call the function for subdirectories
            delete_files "$file"
        fi
    done
}

# Start from the current directory
delete_files "."

echo "Deletion complete."
