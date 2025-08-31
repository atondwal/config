#!/bin/bash
source ~/.openai-key.sh
yad --notification \
    --image=audio-input-microphone \
    --text="Running..." \
    --command="notify-send 'Still running...'" \
    --listen &
YAD_PID=$!

if [[ $1 == "once" ]]; then
  whisper-stream -q -o -s 0.5 -p2 'xargs -0 xdotool type --clearmodifiers' 2>1 >/tmp/asdf
else
  pkill whisper-stream || whisper-stream -q -s 0.5 -p2 'xargs -0 xdotool type --clearmodifiers' 2>1 >/tmp/asdf
fi

kill "$YAD_PID"
