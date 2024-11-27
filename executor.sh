#!/bin/bash

# [TODO] It should get bin_dir from cabal.sandbox.config
export BIN_DIR INCLUDE_DIR LOG_DIR;

PWD=$(pwd)
BIN_DIR=${PWD}/.cabal-sandbox/bin
INCLUDE_DIR=${PWD}/external
LOG_DIR=${PWD}/log

files () {
    git ls-tree HEAD --name-only -r | grep -v -E "external/"
}

build_regexp () {
    echo "($(echo "$@" usage | tr " " "|"))"
}

log () {
    local category=$1
    local name=$2
    echo "tee ${LOG_DIR}/${category}_${name}.log"
}

execute () {
    local executor=$1
    local command_prefix=$2
    local command_regexp=$3
    local parameters=${4:-""}

    # echo "Executor      : $executor"
    # echo "Command prefix: $command_prefix"
    # echo "Command regexp: $command_regexp"
    # echo "Parameters    : $parameters"

    if [[ $parameters == "" ]]; then
        eval "${command_prefix}_usage"
        exit 1
    fi

    mkdir -p "${LOG_DIR}"

    echo "$parameters" | tr " " "\n" | while read -r command; do
        if echo "${command}" | grep -Eqx "${command_regexp}" ; then
            echo "[${command}]"
            ( eval "${command_prefix}_${command}"  ) |& eval "$(log "$executor" "$command")"
        else
            echo "Unknown command ${command}" 
            eval "${command_prefix}_usage"
            exit 1
        fi
    done
}
