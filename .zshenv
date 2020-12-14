# -*- mode: sh; sh-shell: zsh; -*-

export EDITOR=emacsclient
export VISUAL=emacsclient

makeflags=()
typeset -U makeflags
typeset -T MAKEFLAGS makeflags " "

typeset -U path

function add_to_path_if_exists() {
    if [[ -d "$1" ]]; then
        path=($1 $path)
    fi
}

# Basically a lexical scoping block. Nice.
function {
    # You know what, I don't buy the "well it's their CPU so they have every
    # right to intentionally cripple their software on every other CPU" thing.
    # There's no need to handle a company like Intel with kid gloves. Given
    # their resources, they should be able to win without shit like this.
    local VENDOR=$(awk '$1 == "vendor_id" { print $3; exit }' /proc/cpuinfo)
    if [[ $VENDOR == "AuthenticAMD" ]]; then
        export MKL_DEBUG_CPU_TYPE=5
    fi

    # This gets the number of physical cores. In my experience, SMT is
    # counterproductive for heavy-duty numerical computations.
    local NUM_CORES=$(awk '$1 == "cpu" && $2 == "cores" { print $4; exit }' /proc/cpuinfo)
    export GOTO_NUM_THREADS=$NUM_CORES
    export JULIA_NUM_THREADS=$NUM_CORES
    export MKL_NUM_THREADS=$NUM_CORES
    export NUMEXPR_MAX_THREADS=$NUM_CORES
    export NUMEXPR_NUM_THREADS=$NUM_CORES
    export NUMBA_NUM_THREADS=$NUM_CORES
    export OMP_NUM_THREADS=$NUM_CORES
    export OPENBLAS_NUM_THREADS=$NUM_CORES
    export VECLIB_MAXIMUM_THREADS=$NUM_CORES

    # This gets the number of virtual cores. I think SMT will perform better
    # here, but I'm not sure.
    #
    # We export this variable because it gets used by cabal.
    export ncpus="$(nproc --all)"
    makeflags+=("-j$ncpus")

    # Conda
    if [[ -d "$HOME/opt/conda" ]]; then
        local CONDA_DIR="$HOME/opt/conda"
    fi
    if [ -n "${CONDA_DIR+1}" ]; then
        local CONDA="$CONDA_DIR/bin/conda"
        local conda_setup="$($CONDA 'shell.bash' 'hook' 2> /dev/null)"
        if [[ $? -eq 0 ]]; then
            eval "$conda_setup"
        else
            if [[ -f "$CONDA_DIR/etc/profile.d/conda.sh" ]]; then
                . "$CONDA_DIR/etc/profile.d/conda.sh"
            else
                path=("$CONDA_DIR/bin" $path)
            fi
        fi
    fi

    # Curry
    add_to_path_if_exists "$HOME/opt/pakcs-3.2.0/bin"
    add_to_path_if_exists "$HOME/.cpm/bin"

    # Doom Emacs
    add_to_path_if_exists "$HOME/opt/doom-emacs/bin"

    # Haskell
    add_to_path_if_exists "$HOME/.ghcup/bin"
    add_to_path_if_exists "$HOME/.cabal/bin"

    # MATLAB
    if [[ -d "$HOME/opt/MATLAB" ]]; then
        local matlab_dir=$(echo $HOME/MATLAB/*) 2>/dev/null
        if [[ ! -z "$matlab_dir" ]]; then
            path=("$matlab_dir/bin" $path)
        fi
    fi

    # node.js
    if [[ -d "$HOME/.node_modules" ]]; then
        export npm_config_prefix="$HOME/.node_modules"
        path=("$npm_config_prefix/bin" $path)
    fi

    # Nsight
    if [[ -d /opt/cuda ]]; then
        local nsight_dir=$(echo /opt/cuda/nsight-compute*) 2>/dev/null
        if [[ ! -z "$nsight_dir" ]]; then
            path=("$nsight_dir" $path)
        fi
    fi

    # Rust
    add_to_path_if_exists "$HOME/.cargo/bin"
}

# Can't make functions local.
unfunction add_to_path_if_exists

# Make Qt apps work on i3.
export QT_AUTO_SCREEN_SCALE_FACTOR=0

# Enable vsync for OpenGL/Vulkan apps (I'm switching to AMD ASAP; seriously,
# fuck Nvidia).
export __GL_SYNC_TO_VBLANK=1

# Set DISPLAY properly on WSL.
if uname -a | grep -q "microsoft"; then
    export DISPLAY="$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0"
fi
