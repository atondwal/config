#!/bin/bash

dir="${1:-.}"
header_file=$(mktemp)
output_csv="$(mktemp)"

# Get all unique attributes as headers, remove 'user.' prefix
ls "$dir" | xargs -i getfattr -d {} 2>/dev/null |
    grep -o 'user\.[^=]*' |
    sed 's/user\.//' |  # Remove the 'user.' prefix
    sort -u > "$header_file"

# Start CSV with filename header
echo -n "filename," > "$output_csv"
# Add other headers
cat "$header_file" | tr '\n' ',' >> "$output_csv"
echo >> $output_csv

# Process each file
ls "$dir" | while read -r file; do
    # Write filename
    echo -n "$file," >> "$output_csv"
    # Check and write each attribute value
    while read attr; do
        value=$(getfattr -n "user.$attr" "$file" 2>/dev/null | grep "^user.$attr" | cut -d'=' -f2 | tr -d '"')
        if [ -z "$value" ]; then
            # If no attribute, write empty field
            echo -n "," >> "$output_csv"
        else
            # Write attribute value, ensuring CSV compliance
            echo -n "\"$value\"," >> "$output_csv"
        fi
    done < "$header_file"
    # End the line for this file
    echo >> "$output_csv"
done

column -t -s, -o, <$output_csv
rm "$header_file"
rm "$output_csv"
