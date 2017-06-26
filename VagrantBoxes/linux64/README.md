# build MovingBoundary solver for server deployment on centos 7.2

vagrant up
vagrant ssh -c /vagrant/build_server.sh
vagrant ssh -c /vagrant/build.sh
vagrant halt
