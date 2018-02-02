#!/usr/bin/env bash

THIS_DIR="$(cd "$(dirname "$0")"; pwd)"
PROJECT_DIR="$(cd "$(dirname "$0")"; cd ..; pwd)"
WORKSPACE_DIR="$(cd "$(dirname "$0")"; cd ..; cd ..; pwd)"
# echo "dir is $THIS_DIR"
# echo "projectDir is $PROJECT_DIR"
# echo "workspaceDir is $WORKSPACE_DIR"

container_name="vcell-solvers-ide"

(docker container ls | grep $container_name)>/dev/null
if [[ $? -ne 0 ]]; then
	echo "container not created or not running, call ./create.sh first"
	exit 1
fi

echo "starting shell in running container '$container_name', type 'exit' to leave"
docker container exec -it $container_name /bin/bash


