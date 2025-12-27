#!/usr/bin/env python3
import json
import sys
import os
import socket
from datetime import datetime

def main():
    try:
        # Read hook input data from stdin
        input_data = json.load(sys.stdin)
        
        # Extract bash command and context
        command = input_data.get('tool_input', {}).get('command', '')
        cwd = input_data.get('cwd', os.getcwd())
        
        if not command:
            sys.exit(0)
        
        # Get enhanced context
        timestamp = datetime.now().strftime('%Y%m%d %H:%M:%S')
        hostname = socket.gethostname()
        username = os.getenv('USER', 'unknown')
        
        # Log to Claude-specific history file
        history_file = os.path.expanduser('~/.claude_shell_history')
        entry = f"# {timestamp} {username}@{hostname}:{cwd}\n{command}\n"
        with open(history_file, 'a') as f:
            f.write(entry)
        
        # Allow the command to proceed
        sys.exit(0)
        
    except Exception as e:
        # Log error but don't block the command
        print(f"History logging error: {e}", file=sys.stderr)
        sys.exit(0)

if __name__ == "__main__":
    main()