#!/usr/bin/env bash

DIR="$(cd "$(dirname "$0")"; pwd)"
WORKSPACE_DIR="$(cd "$(dirname "$0")"; cd ..; cd ..; pwd)"
echo "workspaceDir is $WORKSPACE_DIR"

cd $DIR

image_name="localhost:5000/vcell-solvers-ide:dev"
container_name="vcell-solvers-ide"

sudo docker build -f ./Dockerfile-ide --tag $image_name .
if [ $(uname) == "Darwin" ]; then 
	IP=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}')
	sudo docker create -it --name $container_name -v "$WORKSPACE_DIR:/workspace" -e DISPLAY=$IP:0 $image_name
elif [ $(uname) == "Linux" ]; then
	sudo docker create -it --name $container_name --net=host -v /tmp/.X11-unix -v "$WORKSPACE_DIR:/workspace" -e DISPLAY $image_name	
fi
sudo docker start $container_name

#
# enabling X11 communication between this host and the Docker container:
#    https://cntnr.io/running-guis-with-docker-on-mac-os-x-a14df6a76efc
#    https://blog.alexellis.io/linux-desktop-on-mac/
#
# requires socat (socket cat)
# > brew install socat
# > (socat TCP-LISTEN:6000,reuseaddr,fork UNIX-CLIENT:\"$DISPLAY\") &
#
# to test ability to run an X program:
#   docker run -e DISPLAY=$IP gns3/xeyes
#
if [ $(uname) == "Darwin" ]; then 
	echo "'socat' is needed to display eclipse user interface running within Docker container on MacOS."
	(ps -ef | grep socat | grep -v grep)>/dev/null
	if [[ $? -ne 0 ]]; then
	   echo "starting socat process in background"
	   echo "(socat TCP-LISTEN:6000,reuseaddr,fork UNIX-CLIENT:\"$DISPLAY\") &"
	   echo "  on Macos, install socat using Homebrew 'brew install socat'"
	   (socat TCP-LISTEN:6000,reuseaddr,fork UNIX-CLIENT:\"$DISPLAY\") &
	else
	   echo "found socat already running"
	fi
fi

