makeflags=()
typeset -U makeflags
typeset -T MAKEFLAGS makeflags " "

typeset -U path

function {
    path+=("$HOME/.local/bin")

    # This gets the number of physical cores. In my experience, SMT is
    # counterproductive for heavy-duty numerical computations.
    local NUM_CORES="${$(/usr/bin/grep -m1 'cpu cores' /proc/cpuinfo)##*: }"
    export GOTO_NUM_THREADS=$NUM_CORES
    export JULIA_NUM_THREADS=$NUM_CORES
    export MKL_NUM_THREADS=$NUM_CORES
    export NUMEXPR_MAX_THREADS=$NUM_CORES
    export NUMBA_NUM_THREADS=$NUM_CORES
    export OMP_NUM_THREADS=$NUM_CORES
    export OPENBLAS_NUM_THREADS=$NUM_CORES
    export VECLIB_MAXIMUM_THREADS=$NUM_CORES

    # This gets the number of virtual cores. I think SMT will perform better
    # here, but I'm not sure.
    makeflags+=("-j$(nproc --all)")

    path+=("$HOME/.ghcup/bin")
    path+=("$HOME/.cabal/bin")

    local CONDA_DIR="$HOME/.local/opt/conda"
    local CONDA="$CONDA_DIR/bin/conda"
    local conda_setup="$($CONDA 'shell.bash' 'hook' 2> /dev/null)"
    if [[ $? -eq 0 ]]; then
	eval "$conda_setup"
    else
	if [[ -f "$CONDA_DIR/etc/profile.d/conda.sh" ]]; then
            . "$CONDA_DIR/etc/profile.d/conda.sh"
	else
            path+=("$CONDA_DIR/bin")
	fi
    fi
}