#!/bin/bash

# Directory for tasks
task_dir="${TASK_DIR:-.}"
mkdir -p "$task_dir"

# Task name and initial status
task_name="$1"
initial_status="${2:-Todo}"  # Default status is 'Todo'

# Create a new task file
touch "$task_dir/$task_name"

# Set the initial status
setfattr -n "user.status" -v "$initial_status" "$task_dir/$task_name"

echo "Task '$task_name' created with status '$initial_status'"
