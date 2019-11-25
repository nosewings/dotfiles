makeflags=()
typeset -U makeflags
typeset -T MAKEFLAGS makeflags " "

# When used like this, the `function` keyword defines an immediately executes an
# anonymous function. This basically amounts to a mechanism for lexical scoping.
function {
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

    alias dotfiles='/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME'

    if [[ -f $ZSH/oh-my-zsh.sh ]]; then
	source $ZSH/oh-my-zsh.sh
    fi
}
