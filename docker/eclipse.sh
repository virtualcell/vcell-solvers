#!/usr/bin/env bash

THIS_DIR="$(cd "$(dirname "$0")"; pwd)"
PROJECT_DIR="$(cd "$(dirname "$0")"; cd ..; pwd)"
WORKSPACE_DIR="$(cd "$(dirname "$0")"; cd ..; cd ..; pwd)"
echo "dir is $THIS_DIR"
echo "projectDir is $PROJECT_DIR"
echo "workspaceDir is $WORKSPACE_DIR"


# https://cntnr.io/running-guis-with-docker-on-mac-os-x-a14df6a76efc
# https://blog.alexellis.io/linux-desktop-on-mac/

# > brew install socat
# > socat TCP-LISTEN:6000,reuseaddr,fork UNIX-CLIENT:\"$DISPLAY\"

# to test ability to run an X program:
#   docker run -e DISPLAY=$IP gns3/xeyes

# (socat TCP-LISTEN:6000,reuseaddr,fork UNIX-CLIENT:\"$DISPLAY\") &

IP=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}')

image_name="localhost:5000/vcell-solvers-ide:dev"
WORKSPACE_DIR=/Users/schaff/Documents/workspace-modular
ECLIPSE="/root/.local/share/umake/ide/eclipse-cpp/eclipse"

docker run -it  \
  -v "$WORKSPACE_DIR:/workspace" -e DISPLAY=$IP:0 $image_name \
  $ECLIPSE -data /workspace
