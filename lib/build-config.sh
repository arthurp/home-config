
if command -v python3 > /dev/null 2> /dev/null; then
    ANY_PYTHON_CMD=python3
else
    ANY_PYTHON_CMD=python
fi

export NPROC="$($ANY_PYTHON_CMD -c 'import multiprocessing; print(multiprocessing.cpu_count())' 2> /dev/null)"
if [ "$NPROC" ]; then
    PARALLEL_LEVEL="$[ NPROC * 7 / 8 ]"
    export PARALLEL_LEVEL="$[ $PARALLEL_LEVEL < 40 ? $PARALLEL_LEVEL : 40 ]"
    export MAKEFLAGS="--jobs=$PARALLEL_LEVEL --load-average=$[ NPROC * 3 / 4 ]"
    export CMAKE_BUILD_PARALLEL_LEVEL="$PARALLEL_LEVEL"
else
    unset NPROC
fi

if command -v ccache > /dev/null 2> /dev/null; then
    export CMAKE_C_COMPILER_LAUNCHER="ccache"
    export CMAKE_CXX_COMPILER_LAUNCHER="ccache"
    export CMAKE_CUDA_COMPILER_LAUNCHER="ccache"
fi
