# build MovingBoundary solver for server deployment on centos 7.2 and gcc 4.8.5-11 or later.

vagrant up

vagrant ssh -c /vagrant/build_server.sh
vagrant ssh -c /vagrant/build_client.sh

cp numerics/build_MB/Solvers/MovingBoundary MovingBoundary_x64

