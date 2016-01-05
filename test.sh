#!/bin/bash

# ANSI styles
STYLE_BOLD="\e[1m"
STYLE_RESET="\e[0m"

# ANSI color (fg)
COLOR_DEFAULT="\e[39m"
COLOR_RED="\e[31m"
COLOR_GREEN="\e[32m"
COLOR_BLACK="\e[30m"

# ANSI color (bg)
COLOR_BG_DEFAULT="\e[49m"
COLOR_BG_RED="\e[41m"
COLOR_BG_GREEN="\e[42m"
COLOR_BG_BLACK="\e[40m"

#
MSG_PASS="${COLOR_GREEN}ok${COLOR_DEFAULT}"
MSG_FAIL="${COLOR_BG_RED}${COLOR_BLACK}failed${COLOR_BG_DEFAULT}${COLOR_DEFAULT}"

#
TEST_CMD="../../ocaml-rename"

#
for TEST_FILE in test/*/*.args; do
    FILE_BASENAME="$(basename ${TEST_FILE})";
    FILE_DIRNAME="$(dirname ${TEST_FILE})";

    TEST_NAME="${FILE_BASENAME%.args}";
    TEST_SET="$(basename ${FILE_DIRNAME})";
    TEST_PATH="${FILE_DIRNAME}/${TEST_NAME}";
    TEST_ARGS="$(cat ${TEST_FILE})"

    TEST_CMD_ARGS="${TEST_CMD} ${TEST_ARGS}";
    cd "${FILE_DIRNAME}"
    $TEST_CMD_ARGS > "${TEST_NAME}.out_actual" 2> "${TEST_NAME}.err_actual"
    cd ../../
    echo -en "[${STYLE_BOLD}${TEST_SET}${STYLE_RESET}/${STYLE_BOLD}${TEST_NAME}${STYLE_RESET}] "
    if (diff "${TEST_PATH}.out_actual" "${TEST_PATH}.out_expected" > "${TEST_PATH}.out_diff" 2>/dev/null &&
        diff "${TEST_PATH}.err_actual" "${TEST_PATH}.err_expected" > "${TEST_PATH}.err_diff" 2>/dev/null); then
        echo -e "\t${MSG_PASS}";
    else
        echo -e "\t${MSG_FAIL} (see ${TEST_PATH}.{out,err}_diff)";
    fi
done