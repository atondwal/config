#!/bin/bash

# Directory for tasks
task_dir="${TASK_DIR:-./tasks}"

# Task name and new status
task_name="$1"
new_status="$2"

# Check if task file exists
if [ -f "$task_dir/$task_name" ]; then
    # Update the status
    setfattr -n "user.status" -v "$new_status" "$task_dir/$task_name"
    echo "Task '$task_name' status updated to '$new_status'"
else
    echo "Error: Task '$task_name' does not exist."
fi
