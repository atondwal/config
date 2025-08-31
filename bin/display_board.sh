#!/bin/bash

# Directory for tasks
task_dir="${TASK_DIR:-./tasks}"

# Get all tasks and their status
echo "Kanban Board"
echo "------------"
for status in Todo Doing Done; do
    echo "$status:"
    for f in $task_dir/*; do
        task_status=$(getfattr -n user.status "$f" 2>/dev/null |head -n2 |tail -n1| cut -d"=" -f2)
        [[ "$task_status" == "\"$status\"" ]] && echo " - $(basename "$f")"
    done
    echo
done
