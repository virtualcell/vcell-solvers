#!/usr/bin/env bash

container_name="vcell-solvers-ide"

sudo docker stop $container_name
sudo docker rm $container_name

