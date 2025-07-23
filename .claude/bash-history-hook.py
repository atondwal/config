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
        
        # Detect shell type and choose appropriate history files
        shell_type = os.path.basename(os.getenv('SHELL', 'bash'))
        
        if 'zsh' in shell_type:
            # For zsh, only log to extended history - let zsh handle its own .zsh_history
            extended_log = os.path.expanduser('~/.zsh_extended_history')
            # Extended history entry with context
            extended_entry = f"# {timestamp} {username}@{hostname}:{cwd}\n{command}\n"
            with open(extended_log, 'a') as f:
                f.write(extended_entry)
        else:
            # For bash, log to both files
            history_file = os.path.expanduser('~/.bash_history')
            extended_log = os.path.expanduser('~/.bash_extended_history')
            
            # Standard history entry (clean for C-r)
            with open(history_file, 'a') as f:
                f.write(f"{command}\n")
            
            # Extended history entry with context
            extended_entry = f"# {timestamp} {username}@{hostname}:{cwd}\n{command}\n"
            with open(extended_log, 'a') as f:
                f.write(extended_entry)
        
        # Allow the command to proceed
        sys.exit(0)
        
    except Exception as e:
        # Log error but don't block the command
        print(f"History logging error: {e}", file=sys.stderr)
        sys.exit(0)

if __name__ == "__main__":
    main()