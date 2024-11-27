#!/bin/bash

# shellcheck disable=SC2086
# Quality assurance script

set -eu

source ./executor.sh

s_cabal () {
    cabal check
}

s_cloc () {
    cloc HEAD --exclude-dir=external
}

s_hlint () {
    ${BIN_DIR}/hlint --cross $hsfiles
}

s_homplexity () {
    ${BIN_DIR}/homplexity-cli $hsfiles
}

s_pylint () {
    pyfiles=$(files | grep -E "\.py$")
    pylint $pyfiles
}

s_shellcheck() {
    shfiles=$(files | grep -E "\.sh$")
    ${BIN_DIR}/shellcheck $shfiles
}

s_commands="cabal cloc cpplint hlint homplexity pylint shellcheck"
s_regexp=$(build_regexp ${s_commands})

s_usage () {
    echo "USAGE: $0 ${s_regexp}"
}

hsfiles=$(files | grep -E "\.hs$" | grep -v -E "PrintOcaml")


if [[ $# -eq 0 ]]; then
    s_parameters=$s_commands
else
    s_parameters="$*"
fi

execute "style" "s" "${s_regexp}" "${s_parameters}"
