#!/usr/bin/env bash

THIS_DIR="$(cd "$(dirname "$0")"; pwd)"
PROJECT_DIR="$(cd "$(dirname "$0")"; cd ..; pwd)"
WORKSPACE_DIR="$(cd "$(dirname "$0")"; cd ..; cd ..; pwd)"

container_name="vcell-solvers-ide"

(docker container ls | grep $container_name)>/dev/null
if [[ $? -ne 0 ]]; then
	echo "container not created or not running, call ./create.sh first"
	exit 1
fi

if [ $(uname) == "Linux" ]; then
	echo "adding xauth cookie"
	XAUTH_COOKIE=$(xauth list | grep `hostname` | head -1)
	docker exec -i $container_name xauth add $XAUTH_COOKIE
fi

echo "starting clion in running container '$container_name'"
CLION="/usr/local/opt/clion/bin/clion.sh"
docker container exec -i $container_name $CLION
