#
# ~/.bashrc
#

[[ $- != *i* ]] && return

# Local secrets (API keys, etc.)
[ -f ~/.bashrc.local ] && source ~/.bashrc.local

alias ls='ls --color=auto'
alias grep='grep --color=auto'
PS1='[\u@\h \W]\$ '

export PATH="$HOME/bin:$HOME/.local/bin:$HOME/.npm-global/bin:$HOME/.config/emacs/bin:$HOME/tools/codeql:$PATH"
export EDITOR=nvim
HISTSIZE=-1
HISTFILESIZE=-1

source /usr/share/fzf/key-bindings.bash
source /usr/share/fzf/completion.bash
[[ -r "/usr/share/z/z.sh" ]] && source /usr/share/z/z.sh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

ash() {
    aws ec2 describe-instances | jq -r '.Reservations[].Instances[] | [.InstanceId, ((.Tags[]? | select(.Key == "Name") | .Value) // "No Name"), .InstanceType, .State.Name, .PublicIpAddress] | @tsv' | grep --color=auto "$1" | awk '{print $5}' | head -n1 | xclip -selection primary
    xclip -selection primary -o
    amazon.sh
    ssh -t amazon
}

journal() { nvim ~/src/wiki/journals/$(date +%Y_%m_%d).md; }
alias jrnl='nvim ~/src/wiki/journals/$(date +%Y_%m_%d).md'
bind -x '"\C-j": "nvim ~/src/wiki/journals/$(date +%Y_%m_%d).md"'
alias d="happy --yolo"

source ~/src/config/.bash_enhanced_history