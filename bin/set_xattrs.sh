#!/bin/bash

# CSV file to read
input_csv="${1:-xattrs.csv}"

# Read the headers (attribute names) excluding the first column (filename)
IFS=',' read -r -a headers < "$input_csv"

tail -n +2 "$input_csv" | while IFS=',' read -ra line; do
    filename=$(echo "${line[0]}" | tr -d ' ')
    touch "$filename"
    for (( i = 1; i < ${#line[@]}; i++ )); do
        attr_name=$(echo "${headers[i]}" | tr -d '"' | tr -d ' ')
        attr_value=$(echo "${line[i]}" | tr -d '"' | tr -d ' ')
        if [ -n "$attr_value" ]; then
            setfattr -n user."$attr_name" -v "$attr_value" "$filename"
        else
            setfattr -x user."$attr_name" "$filename" &>/dev/null
        fi
    done
done
