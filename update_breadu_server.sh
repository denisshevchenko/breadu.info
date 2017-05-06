#!/bin/bash
set -euo pipefail

# This script updates 'breadu-exe' server. Assumed that new executable,
# common food and static files already copied here from the CI-server.
# Assumed that current user can do sudo-commands without password, for simplicity.

# Good practice: check script's correctness after each changing. I recommend 'shellcheck',
# please see https://github.com/koalaman/shellcheck for more info.

readonly EXECUTABLE_NAME=breadu-exe
readonly ROOT_DIR=/home/dshevchenko/breadu-root
readonly PATH_TO_COMMON_FOOD="${ROOT_DIR}"/food/common.csv
readonly PATH_TO_EXECUTABLE="${ROOT_DIR}"/"${EXECUTABLE_NAME}"
readonly PATH_TO_TMP_EXECUTABLE=/tmp/"${EXECUTABLE_NAME}"

readonly SERVER_DEFAULT_PORT=3000
readonly SERVER_ALTERNATIVE_PORT=3010

# Assumed that nginx is already installed.
readonly NGINX_CONFIG=/etc/nginx/sites-enabled/default

# Helper functions ##############################################################
startNewServer() {
    readonly ACTUAL_PORT=$1
    # Start a server in background and pass argumens to it (port and path to the file with common food).
    start-stop-daemon --start -b -q -x "${PATH_TO_EXECUTABLE}" -- -p "${ACTUAL_PORT}" -f "${PATH_TO_COMMON_FOOD}"
}

stopOldServer() {
    readonly ACTUAL_PID=$1
    # Stop running server via its PID.
    start-stop-daemon --stop -q -o --pid "${ACTUAL_PID}"
}

switchNginxToNewServer() {
    readonly SERVER_CURRENT_PORT=$1
    readonly SERVER_NEW_PORT=$2
    # Assumed that ${NGINX_CONFIG} already contains 'proxy_pass' setting,
    # for redirectiong requests from 80 port to our server (127.0.0.1:some_server_port).
    readonly PROXY_PASS="proxy_pass http://127.0.0.1:"
    readonly CURRENT_PROXY_PASS="${PROXY_PASS}""${SERVER_CURRENT_PORT}"
    readonly NEW_PROXY_PASS="${PROXY_PASS}""${SERVER_NEW_PORT}"
    # Replace current 'proxy_pass' setting with the a new one.
    sudo sed -i "s#${CURRENT_PROXY_PASS}#${NEW_PROXY_PASS}#g" "${NGINX_CONFIG}"
    echo "Nginx reloads configuration..."
    sudo nginx -s reload
    # Now Nginx already redirects requests to a new running server.
    echo "Done."
}
#################################################################################

echo "Preparing new executable."
mv -f "${PATH_TO_TMP_EXECUTABLE}" "${PATH_TO_EXECUTABLE}"

echo "Check PID of the current process"
readonly PID_OF_CURRENT_PROCESS=$(pgrep -l breadu-exe | awk '{print $1}')
if [ -z "${PID_OF_CURRENT_PROCESS}" ]; then
    echo "There's no running process, just starting a new one."
    startNewServer "${SERVER_DEFAULT_PORT}"
    switchNginxToNewServer "${SERVER_DEFAULT_PORT}" "${SERVER_DEFAULT_PORT}"
    exit 0
else
    echo "Server already is running, updating it..."
    readonly CURRENT_SERVER_PORT=$(sudo netstat -antulp | grep "${EXECUTABLE_NAME}" | awk '{split($4,a,":"); print a[2]}')
    if [ "${CURRENT_SERVER_PORT}" = "${SERVER_DEFAULT_PORT}" ]; then
        echo "Current server runs on default port, start a new server on alternative port."
        startNewServer "${SERVER_ALTERNATIVE_PORT}"
        switchNginxToNewServer "${SERVER_DEFAULT_PORT}" "${SERVER_ALTERNATIVE_PORT}"
    else
        echo "Current server runs on alternative port, start a new server on default port."
        startNewServer "${SERVER_DEFAULT_PORT}"
        switchNginxToNewServer "${SERVER_ALTERNATIVE_PORT}" "${SERVER_DEFAULT_PORT}"
    fi
    stopOldServer "${PID_OF_CURRENT_PROCESS}"
    exit 0
fi
