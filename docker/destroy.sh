#!/usr/bin/env bash

container_name="vcell-solvers-ide"

docker stop $container_name
docker rm $container_name

