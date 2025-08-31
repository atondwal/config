#!/bin/bash
set -euf -o pipefail
set -x

# Directory for tasks
task_dir="${TASK_DIR:-.}"

# Input CSV file name
input_csv="${INPUT_CSV:-kanban_board.csv}"

# Read headers (statuses) from the first line of the CSV
IFS=',' read -r -a statuses < <(head -n1 "$input_csv")

# Process each row starting from the second
tail -n +2 "$input_csv" | while IFS=',' read -ra line; do
    # Loop through each status and corresponding task
    for (( i = 0; i < ${#line[@]}; i++ )); do
        task_name="${line[i]}"
        [ -z "$task_name" ] && continue  # Skip empty entries
        task_file="$task_dir/$task_name"
        touch $task_file
        setfattr -n "user.status" -v "${statuses[i]}" "$task_file"
    done
done

echo "Updated task statuses based on '$input_csv'"
