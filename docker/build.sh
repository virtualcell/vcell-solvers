#!/usr/bin/env bash

DIR="$(cd "$(dirname "$0")"; pwd)"
echo "dir is $DIR"

cd $DIR

image_name="localhost:5000/vcell-solvers-ide:dev"

docker build -f ./Dockerfile-ide --tag $image_name .

