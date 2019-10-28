ZSH=/usr/share/oh-my-zsh/
ZSH_THEME="ys"

plugins=(git)
ZSH_CACHE_DIR=$HOME/.cache/oh-my-zsh
if [[ ! -d $ZSH_CACHE_DIR ]]; then
  mkdir $ZSH_CACHE_DIR
fi

alias dotfiles='/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME'

export JULIA_NUM_THREADS="${$(/usr/bin/grep -m1 'cpu cores' /proc/cpuinfo)##*: }"

__CONDA_DIR="$HOME/opt/miniconda3"
__CONDA="$__CONDA_DIR/bin/conda"
__conda_setup="$($__CONDA 'shell.bash' 'hook' 2> /dev/null)"
if [[ $? -eq 0 ]]; then
    eval "$__conda_setup"
else
    if [ -f "$__CONDA_DIR/etc/profile.d/conda.sh" ]; then
        . "$__CONDA_DIR/etc/profile.d/conda.sh"
    else
        export PATH="$__CONDA_DIR/bin:$PATH"
    fi
fi
unset __CONDA_DIR
unset __CONDA
unset __conda_setup

if [[ -f $ZSH/oh-my-zsh.sh ]]
   source $ZSH/oh-my-zsh.sh
fi
