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

echo "starting eclipse in running container '$container_name'"
ECLIPSE="/usr/local/opt/eclipse/eclipse"
docker container exec -i $container_name $ECLIPSE -data /workspace
