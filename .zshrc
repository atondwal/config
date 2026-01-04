autoload -Uz compinit
if [[ -n ~/.zcompdump(#qN.mh+24) ]]; then
  compinit
else
  compinit -C
fi

zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'

unsetopt correctall

# Enable command substitution in prompts
setopt PROMPT_SUBST

# Git info - only shows branch name if in a repo
git_info() {
    local branch=$(git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/')
    if [[ -n $branch ]]; then
        # Check if dirty
        if [[ -n $(git status -s 2>/dev/null) ]]; then
            echo " *$branch"
        else
            echo " $branch"
        fi
    fi
}

PROMPT='%F{green}%n@%m%f:%F{blue}%~%f%F{yellow}$(git_info)%f
%(?.%F{white}$.%F{red}%# [%?])%f '

RPROMPT=''

# SSH session gets yellow host to make it obvious you're remote
if [[ -n "$SSH_CONNECTION" ]]; then
  PROMPT='[%F{magenta}%n@%m %F{blue}%~%f]%F{yellow}$(git_info)%f
%(?.%F{white}$.%F{red}%# [%?])%f '
fi



export KEYTIMEOUT=1

# # Source Prezto.
# if [[ -s "${ZDOTDIR:-$HOME}/src/config/.zprezto/init.zsh" ]]; then
#   source "${ZDOTDIR:-$HOME}/src/config/.zprezto/init.zsh"
# fi

#if [ -e /home/atondwal/.nix-profile/etc/profile.d/nix.sh ]; then . /home/atondwal/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

# ZSH_HIGHLIGHT_STYLES[path]='fg=lightblue'
#prompt agnoster

#source /usr/local/bin/virtualenvwrapper.sh

WORDCHARS=${WORDCHARS/\//}
# alias -s pdf="evince"
alias ra=ranger
alias open=xdg-open
alias vn=nvim
export PATH=~/.npm-global/bin:$PATH
d() {
  if [[ $# -eq 1 ]]; then
    tmux new-session -s "$1" claude --allow-dangerously-skip-permissions "/rename $1"
  else
    claude --allow-dangerously-skip-permissions "$*"
  fi
}
resume() {tmux new-session -s "$1" claude --allow-dangerously-skip-permissions "/resume $1"}
p() { claude --allow-dangerously-skip-permissions -p "$*"; }
alias ack=ag
#alias vw="nvim +VimwikiIndex"
# Emacsclient aliases
alias ec='emacsclient -c'           # new GUI frame
alias et='emacsclient -t'           # terminal frame
em() { emacsclient -c -a '' "$@"; } # start daemon if needed + open
autoload -U zmv
bindkey '^R' history-incremental-pattern-search-backward
alias iwsplit="sudo iw dev wlp3s0 interface add wlp3s1 type station"

bindkey "^?" backward-delete-char
bindkey "^W" backward-kill-word
bindkey "^H" backward-delete-char
bindkey "^U" backward-kill-line
bindkey "æ" forward-word
bindkey "â" backward-word

nv() {
  if [[ -z $NVIM_LISTEN_ADDRESS ]]; then
    # not running inside nvim
    nvim "$@"
  else
    python -c "from neovim import attach; import os; vim = attach('socket', path='$NVIM_LISTEN_ADDRESS'); vim.command('tabe '+os.path.abspath('$1'));"
  fi
}

# Set window title to command just before running it and the current working
# directory after returning from a command.
preexec() { printf "\x1b]0;%s\x07" "$1"; }
precmd() { printf "\x1b]0;%s\x07" "$PWD" }

mkcd ()
{
      mkdir -p -- "$1" &&
      cd -P -- "$1"
}

GR() { git rev-parse --show-toplevel 2>/dev/null; }

# fzf functions "{{{
# fd - cd to selected directory
fd() {
  local dir
  dir=$(find ${1:-.} -path '*/\.*' -prune \
                  -o -type d -print -maxdepth 3 2> /dev/null | fzf +m) &&
  cd "$dir"
}

# cf - fuzzy cd from anywhere
# ex: cf word1 word2 ... (even part of a file name)
# zsh autoload function
cf() {
  local file

  file="$(locate -Ai -0 $@ | grep -z -vE '~$' | fzf --read0 -0 -1)"

  if [[ -n $file ]]
  then
     if [[ -d $file ]]
     then
        cd -- $file
     else
        cd -- ${file:h}
     fi
  fi
}
# fzf functions "}}}

# uses too much power
# eval $(thefuck --alias)

NPM_PACKAGES="${HOME}/.npm-packages"
export PATH="$PATH:$NPM_PACKAGES/bin"

# Preserve MANPATH if you already defined it somewhere in your config.
# Otherwise, fall back to `manpath` so we can inherit from `/etc/manpath`.
export MANPATH="${MANPATH-$(manpath)}:$NPM_PACKAGES/share/man"

function sex() {
  vim - -esbnN -c $* -c 'w!/dev/fd/3|q!' 3>&1 >/dev/null
}

if [ -n "$NVIM" ]; then
    export VISUAL="nvr -cc split --remote-wait +'setlocal bufhidden=wipe'"
else
    export VISUAL="nvim"
fi
export EDITOR="$VISUAL"
alias e=$EDITOR

fancy-ctrl-z () {
  if [[ $#BUFFER -eq 0 ]]; then
    if [[ `( jobs ) | grep suspended | wc -l` -lt 3 ]]; then
      fg %- || fg %+
    else
      fg %`( jobs ) | fzf | sed 's/.*\[\([0-9]\+\)\].*/\1/'`
    fi
    zle redisplay
  else
    zle push-input
  fi
}

do-bg () {
  bg
}

zle -N fancy-ctrl-z
zle -N do-bg
bindkey '^Z' fancy-ctrl-z
bindkey '^Q' do-bg

autoload -Uz bracketed-paste-magic
zle -N bracketed-paste bracketed-paste-magic

autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

bindkey '^A' beginning-of-line
bindkey '^E' end-of-line
# Map Alt+Left to move back one word
bindkey '^[b' backward-word
bindkey '\e[1;3D' backward-word
# Map Alt+Right to move forward one word
bindkey '^[f' forward-word
bindkey '\e[1;3C' forward-word

eval "$(direnv hook zsh)"
eval "$(zoxide init zsh)"
eval "$(fzf --zsh)"

# Custom Ctrl-F: search Claude shell history
fzf-claude-history-widget() {
  local selected
  setopt localoptions noglobsubst noposixbuiltins pipefail no_aliases noglob nobash_rematch 2> /dev/null
  selected="$(grep -v '^#' ~/.claude_shell_history 2>/dev/null | tac | awk '!seen[$0]++' |
    FZF_DEFAULT_OPTS=$(__fzf_defaults "" "--scheme=history --bind=ctrl-r:toggle-sort --wrap-sign '\t↳ ' --highlight-line ${FZF_CTRL_R_OPTS-} --query=${(qqq)LBUFFER} +m") \
    FZF_DEFAULT_OPTS_FILE='' $(__fzfcmd))"
  if [[ -n "$selected" ]]; then
    LBUFFER="$selected"
  fi
  zle reset-prompt
}

zle -N fzf-claude-history-widget
bindkey '^F' fzf-claude-history-widget

# Edit command line in $EDITOR (vim)
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^G' edit-command-line

# Zoxide with fzf integration
# Use Ctrl+S to interactively search directories from zoxide database
__zoxide_fzf() {
  local dir
  dir=$(zoxide query -l | fzf --height 40% --reverse --preview 'ls -la {}'  --preview-window right:50%) && cd "$dir"
  zle reset-prompt
}
zle -N __zoxide_fzf
bindkey '^S' __zoxide_fzf

function run-previous-command() {
    zle up-history          # Move to the previous command in history
    zle accept-line         # Execute it
}
zle -N run-previous-command  # Register the widget
bindkey '^P' up-history
bindkey '^N' down-history
setopt interactivecomments

groq() {
    local wrapped_binary="tgpt"

    local -a option_args=()
    local -a non_option_args=()

    for arg in "$@"; do
        if [[ "$arg" == -* ]]; then
            option_args+=("$arg")
        else
            non_option_args+=("$arg")
        fi
    done

    # Call the wrapped binary with all non-option arguments quoted
    $wrapped_binary  --provider groq --key "$GROQ_API_KEY" --model llama3-70b-8192 ${option_args[@]} "${non_option_args[*]}"
}

alias g=groq

taiga() {
  source "$(GR)/.env"
  curl -s -H "Cookie: $TAIGA_API_COOKIE" "https://taiga.ant.dev/api/$1"
}

# Completion for ~/bin/jobs
_jobs_complete() {
  local state
  local -a commands
  commands=(
    'list:List jobs in environment'
    'cancel:Cancel a job'
    'resubmit:Resubmit a problem'
    'resubmit-incomplete:Resubmit all incomplete'
    '--help:Show help'
  )

  _arguments -C \
    '1: :->cmd' \
    '2: :->arg1' \
    '3: :->arg2' \
    '*: :->rest'

  case $state in
    cmd)
      _describe -t commands 'command' commands
      # Also complete job names for direct access
      local -a jobs
      if [[ -f ~/.cache/taiga_jobs ]]; then
        jobs=(${(f)"$(cat ~/.cache/taiga_jobs)"})
        _describe -t jobs 'job' jobs
      fi
      ;;
    arg1)
      case $words[2] in
        list)
          # Complete environment names
          if [[ -f ~/.cache/taiga_envs ]]; then
            local -a envs
            envs=(${(f)"$(cat ~/.cache/taiga_envs)"})
            _describe -t envs 'environment' envs
          fi
          ;;
        cancel|resubmit-incomplete)
          # Complete job names
          if [[ -f ~/.cache/taiga_jobs ]]; then
            local -a jobs
            jobs=(${(f)"$(cat ~/.cache/taiga_jobs)"})
            _describe -t jobs 'job' jobs
          fi
          ;;
        resubmit)
          _arguments '-n[Number of times]'
          # Complete job names or problem names
          if [[ -f ~/.cache/taiga_jobs ]]; then
            local -a jobs
            jobs=(${(f)"$(cat ~/.cache/taiga_jobs)"})
            _describe -t jobs 'job' jobs
          fi
          ;;
      esac
      ;;
    arg2)
      if [[ $words[2] == "resubmit" ]]; then
        # Could be -n value, job name, or problem name
        if [[ $words[3] == "-n" ]]; then
          _message 'count'
        fi
      fi
      ;;
  esac
}
compdef _jobs_complete jbs

# Refresh taiga completion cache
taiga-cache() {
  source "$(git rev-parse --show-toplevel 2>/dev/null)/.env" 2>/dev/null || source ~/.env
  local base="https://taiga.ant.dev/api"
  local cookie="Cookie: $TAIGA_API_COOKIE"
  mkdir -p ~/.cache

  # Cache environments
  curl -s -H "$cookie" "$base/environments" | jq -r '.data[]?.name // .[]?.name' > ~/.cache/taiga_envs

  # Cache jobs from all environments
  local env_ids=$(curl -s -H "$cookie" "$base/environments" | jq -r '.data[]?.id // .[]?.id')
  : > ~/.cache/taiga_jobs
  for eid in ${(f)env_ids}; do
    curl -s -H "$cookie" "$base/jobs?environment_id=$eid" | jq -r '.[]?.name' >> ~/.cache/taiga_jobs
  done
  echo "Cached $(wc -l < ~/.cache/taiga_envs) envs, $(wc -l < ~/.cache/taiga_jobs) jobs"
}

ash () { aws ec2 describe-instances | jq -r '.Reservations[].Instances[] | [.InstanceId, ((.Tags[]? | select(.Key == "Name") | .Value) // "No_Name"), .InstanceType, .State.Name, .PublicIpAddress] | @tsv' |  grep "$1" | head -n1 | awk '{print $5}' | xclip -selection primary ; xclip -selection primary -o ; amazon.sh; ssh -t amazon}

# Lazy load NVM - only initialize when first used
export NVM_DIR="$HOME/.nvm"
nvm() {
  unfunction nvm node npm npx
  [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
  [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
  nvm "$@"
}
node() { nvm; node "$@"; }
npm() { nvm; npm "$@"; }
npx() { nvm; npx "$@"; }
# ... existing shell config
# export TRANSLUCE_HOME=/home/atondwal/src/observatory
# source $TRANSLUCE_HOME/lib/lucepkg/scripts/shellenv.sh
export XDG_DATA_DIRS=$HOME/.nix-profile/share:$XDG_DATA_DIRS
export FIREWORKS_API_KEY="fw_3ZeMHAECNxLfeTRG7zxvfDxL"

# Source enhanced history logging
source ~/src/config/.zsh_enhanced_history
eval "$(dircolors)"
alias ls='ls --color=auto'


dbash() { docker exec -it ${1:-$(docker ps | head -n 2 | tail -n 1 | cut -d " " -f 1)} /bin/bash; }
alias tb='f() { docker build --build-context taiga=../../ -t "$(basename $(pwd))" -f Dockerfile .; }; f'
alias ttp='f() { tb; docker tag "$(basename $(pwd))" "us-east1-docker.pkg.dev/gcp-taiga/dmodel/$(basename $(pwd)):$1"; docker push "us-east1-docker.pkg.dev/gcp-taiga/dmodel/$(basename $(pwd)):$1"; }; f'

alias oc=opencode
alias cc=claude
alias lq=logseq
alias say=espeak

# bun completions
[ -s "/home/atondwal/.bun/_bun" ] && source "/home/atondwal/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# Clean up mlterm log on exit
if [[ "$TERM" == "mlterm" ]]; then
  trap 'pts=$(tty | grep -oE "[0-9]+$"); rm -f ~/.mlterm/pts_${pts}-*.log 2>/dev/null' EXIT
fi

stty stop ''; stty start ''
