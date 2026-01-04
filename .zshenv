skip_global_compinit=1
export PATH="/home/atondwal/.local/bin:/home/atondwal/bin:/opt/ghc/7.8.4/bin:/home/atondwal/.cabal/bin":$PATH:/home/atondwal/.gem/ruby/2.3.0/bin:/home/atondwal/src/go/bin:/home/atondwal/.nix-profile/bin
export EDITOR='nvim'
export VISUAL=nvim
export WORKON_HOME=$HOME/.virtualenvs
export GOPATH=$HOME/src/go/
export REPLYING_ALL=False
# for steam
#export LD_PRELOAD='/usr/$LIB/libstdc++.so.6 /usr/$LIB/libgcc_s.so.1 /usr/$LIB/libxcb.so.1 /usr/$LIB/libgpg-error.so'

# Source local secrets if present
[[ -f ~/.zshenv.local ]] && source ~/.zshenv.local

# Begin added by argcomplete
fpath=( /usr/lib/python3.13/site-packages/argcomplete/bash_completion.d "${fpath[@]}" )
# End added by argcomplete
. "$HOME/.cargo/env"
