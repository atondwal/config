#!/bin/bash

# Directory for tasks
task_dir="${TASK_DIR:-.}"

# Output CSV file name
output_csv="${OUTPUT_CSV:-kanban_board.csv}"

# Temporary directory for processing
temp_dir=$(mktemp -d)

# Collect all statuses by reading xattrs
statuses=()
for f in "$task_dir"/*; do
    [ -e "$f" ] || continue  # Skip if no files
    status=$(getfattr -n user.status "$f" 2>/dev/null | head -n2 | tail -n1 | cut -d'=' -f2 | tr -d '"' | tr -d ' ')
    if [[ ! " ${statuses[@]} " =~ " $status " ]]; then
        statuses+=("$status")
    fi
done

# Sort statuses for consistent column ordering
IFS=$'\n' statuses=($(sort <<<"${statuses[*]}"))
unset IFS

# Prepare a temporary file for each status
for status in "${statuses[@]}"; do
    touch "$temp_dir/$status"
done

# Populate temporary files with tasks of corresponding status
for f in "$task_dir"/*; do
    [ -e "$f" ] || continue
    status=$(getfattr -n user.status "$f" 2>/dev/null | head -n2 | tail -n1 | cut -d'=' -f2 | tr -d '"' | tr -d ' ')
    echo "$(basename "$f")" >> "$temp_dir/$status"
done

# Create the CSV header
echo "${statuses[*]}" | tr ' ' ',' > "$output_csv"

# Merge all files column-wise into CSV
paste -d, $(printf "%s " "${statuses[@]/#/$temp_dir/}") >> "$output_csv"

# Clean up
rm -r "$temp_dir"

echo "Kanban board saved to $output_csv"
cat "$output_csv"
