if [[ -d /usr/share/oh-my-zsh/ ]]; then
    ZSH="/usr/share/oh-my-zsh/"
else
    ZSH="$HOME/.oh-my-zsh/"
fi
ZSH_THEME="ys"

plugins=(git)
ZSH_CACHE_DIR=$HOME/.cache/oh-my-zsh
if [[ ! -d $ZSH_CACHE_DIR ]]; then
    mkdir $ZSH_CACHE_DIR
fi

alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias less='less -RFX'
alias pakcs="rlwrap $HOME/opt/pakcs-3.2.0/bin/pakcs"
alias rg='rg --color always'

if [[ -f $ZSH/oh-my-zsh.sh ]]; then
    source $ZSH/oh-my-zsh.sh
fi
