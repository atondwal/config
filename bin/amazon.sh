#!/bin/bash

# Check if an IP address argument is provided
if [ "$#" -eq 1 ]; then
    IP=$1
else
    # Get the IP from the clipboard if no argument is given
    IP=$(xclip -o)
fi

# Backup the original ssh config file
cp ~/.ssh/config ~/.ssh/config.bak

OLD_HOST=`sed -n "/^Host amazon$/,/Hostname /{s/Hostname \(.*\)/\1/p}" < ~/.ssh/config`
OLD_HOST=`echo "$OLD_HOST" | tr -d '[:space:]'`

# Use sed to modify the Hostname for the host "amazon"
sed -i "/^Host amazon$/,/Hostname /{s/Hostname .*/Hostname $IP/}" ~/.ssh/config

echo "Hostname updated to $IP from $OLD_HOST for host 'amazon' in .ssh/config"
